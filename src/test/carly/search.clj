(ns test.carly.search
  "Functions for running concurrent workers to search for valid worldlines."
  (:require
    [clojure.test :as ctest]
    [test.carly.op :as op]
    [test.carly.world :as world])
  (:import
    (java.util.concurrent
      PriorityBlockingQueue
      TimeUnit)))


(defn compare-futures
  "Ranks two worlds by the number of possible futures they have. Worlds with
  fewer futures will rank earlier."
  [a b]
  (compare (:futures a) (:futures b)))


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
                  (when-let [end (world/run-linear world mark-visited!)]
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
    (println "Running" n "workers to search worldlines...")
    (let [result (promise)
          workers (repeatedly n #(spawn-worker! queue report-fn visited result))]
      (dorun workers)
      (run! deref workers)
      (if (empty? queue)
        (println "Work queue is empty!")
        (println "Work queue has" (count queue) "remaining states"))
      @result)))


(defn find-valid-worldline
  "Run `thread-count` worker threads to search through worldlines starting from
  the given `model` to find valid linearizations of the `thread-results`."
  [thread-count model thread-results report-fn]
  (let [visited (atom #{})
        origin (world/initialize model thread-results)
        queue (PriorityBlockingQueue. 20 compare-futures)]
    (printf "Initialized origin world. Searching for valid linearization among %s worldlines...\n"
            (:futures origin))
    (flush)
    (.offer queue origin)
    (let [start (System/nanoTime)
          valid-world (run-workers! thread-count queue report-fn visited)
          elapsed (/ (- (System/nanoTime) start) 1000000.0)]
      {:world valid-world
       :visited (count @visited)
       :threads thread-count
       :elapsed elapsed})))
