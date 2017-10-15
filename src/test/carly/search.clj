(ns test.carly.search
  "Functions for running concurrent workers to search for valid worldlines."
  (:require
    [clojure.test :as ctest]
    [test.carly.world :as world])
  (:import
    (java.util.concurrent
      PriorityBlockingQueue
      TimeUnit)))


(defn- compare-futures
  "Ranks two worlds by the number of possible futures they have. Worlds with
  fewer futures will rank earlier."
  [a b]
  (compare (:futures a) (:futures b)))


(defn- run-linear
  "Steps a world forward to completion along a linear track. Returns a valid
  terminal world if the operations end in a valid state, otherwise nil. Calls
  `f` with each world visited."
  [world f]
  (when world
    (f world)
    (if (world/end-of-line? world)
      ; Made it to the end of the world line with consistent results.
      world
      ; Step world forward. A nil here means the next operation result
      ; is invalid, so the observed worldline is inconsistent with the
      ; model.
      (recur (world/step world) f))))


(defn- spawn-worker!
  "Poll the queue for a world, calculate next states, and add valid ones back
  into the queue. The `result` promise will be delivered with the first valid
  terminal world found. Once `result` has been realized, the loop will exit."
  [^PriorityBlockingQueue queue report-fn visited result]
  (future
    (loop []
      (when-not (realized? result)
        (if-let [world (.poll queue 100 TimeUnit/MILLISECONDS)]
          (let [mark-visited! #(swap! visited conj (world/visit-key %))
                visited? #(contains? @visited (world/visit-key %))]
            (when-not (visited? world)
              ; Add world to visited set.
              (mark-visited! world)
              ; Compute steps.
              (binding [ctest/report report-fn]
                (if (<= (:futures world) 1)
                  ; Optimization to run the linear sequence directly when there is only one
                  ; possible future worldline.
                  (when-let [end (run-linear world mark-visited!)]
                    (deliver result end))
                  ; Otherwise, calculate the next possible states and add any unvisited
                  ; ones to the queue.
                  (->> (world/next-steps world)
                       (remove visited?)
                       (run! #(.offer queue %))))))
            (recur))
          ; Didn't find a world; if the queue is still empty, deliver nil.
          (when (empty? queue)
            (deliver result nil)))))))


(defn- run-workers!
  "Run a collection of worker threads to consume the given queue of worlds.
  Blocks and returns the first valid world found, or nil, once all the threads
  have terminated."
  [n queue report-fn visited]
  (when-not (empty? queue)
    (let [result (promise)
          workers (repeatedly n #(spawn-worker! queue report-fn visited result))]
      (dorun workers)
      (run! deref workers)
      @result)))


(defn- linear-search
  "Run a world directly on-thread to perform a linear search. Only appropriate
  when the world has a single worldline left."
  [origin]
  (let [visited (volatile! 0)
        reports (volatile! [])
        start (System/nanoTime)
        valid-world (binding [ctest/report #(vswap! reports conj %)]
                      (run-linear origin (fn [world] (vswap! visited inc))))
        elapsed (/ (- (System/nanoTime) start) 1000000.0)]
    {:world valid-world
     :threads 1
     :futures 1
     :visited @visited
     :reports @reports
     :elapsed elapsed}))


(defn- parallel-search
  "Run up to `thread-count` worker threads to search through worldlines
  starting from the given `origin` world to find valid linearizations of the
  pending operations."
  [origin thread-count]
  (let [visited (atom #{})
        reports (atom [])
        queue (doto (PriorityBlockingQueue. 20 compare-futures)
                (.offer origin))
        start (System/nanoTime)
        valid-world (run-workers! thread-count queue (partial swap! reports conj) visited)
        elapsed (/ (- (System/nanoTime) start) 1000000.0)]
      {:world valid-world
       :threads thread-count
       :futures (:futures origin)
       :visited (count @visited)
       :reports @reports
       :elapsed elapsed}))


(defn search-worldlines
  "Run up to `thread-count` worker threads to search through worldlines
  starting from the given `model` to find valid linearizations of the
  `thread-results`."
  [thread-count model thread-results]
  (when (empty? thread-results)
    (throw (RuntimeException.
             "Cannot search the worldless void (thread results were empty)")))
  (let [origin (world/initialize model thread-results)]
    (if (= 1 (:futures origin))
      (linear-search origin)
      (parallel-search origin thread-count))))
