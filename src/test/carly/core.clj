(ns test.carly.core
  "Suite of tests to verify that a given system implementation conforms to the
  spec during a sequence of operations performed on it."
  (:require
    [clojure.test :as ctest]
    [clojure.test.check.generators :as gen]
    [com.gfredericks.test.chuck :as chuck]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [test.carly.op :as op]
    [test.carly.world :as world])
  (:import
    (java.util.concurrent
      PriorityBlockingQueue
      TimeUnit)))


; TODO: move to op ns?
(defmacro defop
  "Defines a new specification for a system operation test."
  [op-name attr-vec & forms]
  (let [defined (zipmap (map first forms) forms)]
    (when-let [unknown-forms (seq (dissoc defined 'gen-args 'apply-op 'check 'update-model))]
      (throw (ex-info "Unknown forms defined in operation body"
                      {:unknown (map first unknown-forms)})))
    `(do
       (defrecord ~op-name
         ~attr-vec

         op/TestOperation

         ~(or (defined 'apply-op) '(apply-op [op system] nil))
         ~(or (defined 'check) '(check [op model result] true))
         ~(or (defined 'update-model) '(update-model [op model] model)))

       (defn ~(symbol (str "gen->" (name op-name)))
         ~(str "Constructs a " (name op-name) " operation generator.")
         ~@(if-let [[_ args & body] (defined 'gen-args)]
             [args
              (cond
                (and (= 1 (count body))
                     (vector? (first body)))
                  `(gen/fmap
                     (partial apply ~(symbol (str "->" (name op-name))))
                     (gen/tuple ~@(first body)))
                (and (= 1 (count body))
                     (map? (first body)))
                  `(gen/fmap
                     ~(symbol (str "map->" (name op-name)))
                     (gen/hash-map ~@(apply concat (first body))))
                :else
                  `(gen/fmap
                     ~(symbol (str "map->" (name op-name)))
                     (do ~@body)))]
             [['context]
              `(gen/return (~(symbol (str "->" (name op-name)))))])))))


(defop Wait
  [duration]

  (gen-args
    [_]
    [(gen/choose 1 100)])

  (apply-op
    [this system]
    (Thread/sleep duration)))



;; ## Linear Test

(defn check-system
  "Uses generative tests to validate the behavior of a system under a linear
  sequence of operations.

  Takes a test message, a no-arg constructor function which will produce a new
  system for testing, and a function which will return a vector of operation
  generators when called with the test context. The remaining options control
  the behavior of the tests:

  - `context-gen` generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `on-stop`     side-effecting function to call on the system after testing"
  [message constructor op-generators
   & {:keys [context-gen init-model iterations on-stop]
      :or {context-gen (gen/return {})
           init-model (constantly {})
           iterations 100}}]
  {:pre [(fn? constructor)]}
  (checking message (chuck/times iterations)
    [context context-gen
     ops (gen/not-empty (gen/list (gen/one-of (op-generators context))))]
    (let [system (constructor)]
      (try
        (let [results (op/apply-ops! system ops)]
          (world/run-linear (world/initialize (init-model context) {0 results})))
        (finally
          (when on-stop
            (on-stop system)))))))



;; ## Concurrent Test

(defn- compare-futures
  "Ranks two worlds by the number of possible futures they have. Worlds with
  fewer futures will rank earlier."
  [a b]
  (compare (:futures a) (:futures b)))


(defn- spawn-worker
  "Poll the queue for a world, calculate next states, and add valid ones back
  into the queue. The `result` promise will be delivered with the first valid
  terminal world found. One `result` has been realized, the loop will exit."
  [^PriorityBlockingQueue queue visited result]
  (future
    (loop []
      (when-not (realized? result)
        (if-let [world (.poll queue 1 TimeUnit/SECONDS)]
          (do
            (when-not (contains? @visited (world/visit-key world))
              ; Add world to visited set.
              (swap! visited conj (world/visit-key world))
              ; Compute steps.
              (let [start (System/nanoTime)
                    value
              (binding [ctest/report (constantly nil)]
                (if (<= (:futures world) 1)
                  ; Optimization to run the linear sequence directly when there is only one
                  ; possible future worldline.
                  (when-let [end (world/run-linear world)]
                    (deliver result end))
                  ; Otherwise, calculate the next possible states and add any unvisited
                  ; ones to the queue.
                  (->> (world/next-steps world)
                       (remove (comp @visited world/visit-key))
                       (run! #(.offer queue %)))))]
                ;(printf "Work task %.3f ms\n" (/ (- (System/nanoTime) start) 1000000.0))
                ;(flush)
                value))
            (recur))
          ; Didn't find a world; if the queue is still empty, deliver nil.
          (when (empty? queue)
            (deliver result nil)))))))


(defn- run-workers!
  "Run a collection of worker threads to consume the given queue of worlds.
  Blocks and returns the first valid world found, or nil, once all the threads
  have terminated."
  [n queue visited]
  (when-not (empty? queue)
    (println "Running" n "workers to search worldlines...")
    (let [result (promise)
          workers (repeatedly n #(spawn-worker queue visited result))]
      (dorun workers)
      (run! deref workers)
      (if (empty? queue)
        (println "Work queue is empty!")
        (println "Work queue has" (count queue) "remaining states"))
      @result)))


(defn- run-test-iteration
  [constructor init-model context op-seqs search-threads on-stop]
  (let [system (constructor)]
    (try
      (let [start (System/nanoTime)
            thread-results (op/run-threads! system op-seqs)
            elapsed (/ (- (System/nanoTime) start) 1000000.0)
            _ (do (printf "Ran operations in %.2f ms\n" elapsed) (flush))
            start (System/nanoTime)
            origin (world/initialize (init-model context) thread-results)
            visited (atom #{})
            queue (PriorityBlockingQueue. 20 compare-futures)]
        (printf "Initialized world in %.3f ms. Searching for valid linearization among %s worldlines...\n"
                (/ (- (System/nanoTime) start) 1000000.0)
                (:futures origin))
        (flush)
        (.offer queue origin)
        (let [start (System/nanoTime)
              valid-world (run-workers! search-threads queue visited)
              elapsed (/ (- (System/nanoTime) start) 1000000.0)]
          (if valid-world
            (let [message (format "Found valid worldline in %.2f ms after visiting %d worlds"
                                  elapsed (count @visited))]
              (println message)
              ;(prn (:history valid-world))
              (flush)
              (let [valid-history? (loop [model (init-model context)
                                          ops (:history valid-world)]
                                     (if-let [op (first ops)]
                                       (if (op/check op model (::op/result op))
                                         (recur (op/update-model op model) (rest ops))
                                         false)
                                       true))]
                (ctest/is valid-history?))
              (ctest/do-report
                {:type :pass
                 :message message
                 :expected 'linearizable
                 :actual (:history valid-world)})
              true)
            (let [message (format "Exhausted worldlines in %.2f ms after visiting %d worlds"
                                  elapsed (count @visited))]
              (println message)
              (flush)
              (ctest/do-report
                {:type :fail
                 :message message
                 :expected 'linearizable
                 :actual thread-results})
              false))))
      (finally
        (when on-stop
          (on-stop system))))))


(defn check-system-concurrent
  "Uses generative tests to validate the behavior of a system under multiple concurrent
  threads of operations.

  Takes a test message, a no-arg constructor function which will produce a new
  system for testing, and a function which will return a vector of operation
  generators when called with the test context. The remaining options control
  the behavior of the tests:

  - `context-gen` generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `repetitions` number of times to run per generation to ensure repeatability
  - `max-concurrency` maximum number of operation threads to run in parallel
  - `search-threads` number of threads to run to search for valid worldlines
  - `on-stop`     side-effecting function to call on the system after testing

  Returns the results of the generative tests."
  [message constructor op-generators
   & {:keys [context-gen init-model iterations repetitions max-concurrency search-threads on-stop]
      :or {context-gen (gen/return {})
           init-model (constantly {})
           iterations 20
           repetitions 10
           max-concurrency 4
           search-threads (. (Runtime/getRuntime) availableProcessors)}}]
  {:pre [(fn? constructor)]}
  (checking message (chuck/times iterations)
    [context context-gen
     num-threads (gen/choose 2 max-concurrency)
     op-seqs (-> (gen->Wait context)
                 (cons (op-generators context))
                 (gen/one-of)
                 (gen/list)
                 (gen/not-empty)
                 (gen/vector num-threads))]
    (printf "\nStarting test iteration of %d repetitions with %d ops across %d threads:\n"
            repetitions
            (count (apply concat op-seqs))
            (count op-seqs))
      #_(run! prn op-seqs)
    (loop [i 0]
      (if (<= repetitions i)
        true
        (do
          (println "\nTest repetition" (inc i))
          (if (run-test-iteration constructor init-model context op-seqs search-threads on-stop)
            (recur (inc i))
            false))))))
