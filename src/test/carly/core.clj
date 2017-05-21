(ns test.carly.core
  "Suite of tests to verify that a given system implementation conforms to the
  spec during a sequence of operations performed on it."
  (:require
    [clojure.test :as ctest]
    [clojure.test.check.generators :as gen]
    [com.gfredericks.test.chuck :as chuck]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]))


(defprotocol TestOperation
  "Protocol for a test operation on a system."

  (apply-op
    [operation system]
    "Apply the operation to the system, returning a result value.")

  (check
    [operation model result]
    "Validate an operation given the model state and the response from the
    system being tested. May include `clojure.test/is` assertions, and should
    return a boolean value indicating overall success or failure.")

  (update-model
    [operation model]
    "Apply an update to the model based on the operation."))


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

         TestOperation

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



;; ## Operation Testing

(defn apply-ops!
  "Apply a sequence of operations to a system, returning a vector of pairs of
  the operations with their results."
  [system ops]
  (reduce
    (fn [results op] (conj results [op (apply-op op system)]))
    []
    ops))


(defn run-ops!
  "Applies a sequence of operations in a separate thread. Returns a promise for
  the results of the application."
  [latch system ops]
  (let [results (promise)]
    (doto (Thread.
            (fn apply-ops-thread []
              @latch
              (deliver results (apply-ops! system ops)))
            (str "test.carly/" (Integer/toHexString (System/identityHashCode ops))))
      (.start))
    results))


(defn run-threads!
  "Run each of the given operation sequences in a separate thread. Returns a
  vector of the operation results for each thread."
  [system op-seqs]
  (let [latch (promise)
        threads (map (partial run-ops! latch system) op-seqs)]
    (dorun threads)
    (deliver latch :start)
    ; TODO: timeout on deref?
    (mapv deref threads)))


(defn valid-results?
  "Determines whether the given sequence of operations produced valid results
  when applied to the system. Returns true if the system behavior is valid."
  [model ops]
  (loop [model model
         ops ops]
    (if-let [[op result] (first ops)]
      (if (check op model result)
        (recur (update-model op model) (rest ops))
        false)
      true)))



;; ## Linear Test

(defn check-system
  "Uses generative tests to validate the behavior of a system under a linear
  sequence of operations.

  Takes a test message, a no-arg constructor function which will produce a new
  system for testing, and a function which will return an operation generator
  when called with the test context. The remaining options control the behavior
  of the tests:

  - `context-gen` generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `on-stop`     side-effecting function to call on the system after testing"
  [message constructor op-gen
   & {:keys [context-gen init-model iterations on-stop]
      :or {context-gen (gen/return {})
           init-model (constantly {})
           iterations 100}}]
  {:pre [(fn? constructor)]}
  (checking message (chuck/times iterations)
    [context context-gen
     ops (gen/not-empty (gen/list (op-gen context)))]
    (let [system (constructor)]
      (try
        (valid-results?
          (init-model context)
          (apply-ops! system ops))
        (finally
          (when on-stop
            (on-stop system)))))))



;; ## Concurrent Test

(defn- thread-children
  "Calculates child nodes for a set of possible operation interleavings."
  [{:keys [ordering threads]}]
  (map
    (fn [[i [op & more]]]
      {:ordering (conj ordering op)
       :threads (if (seq more)
                  (assoc threads i more)
                  (dissoc threads i))})
    threads))


(defn interleavings
  "Calculate a lazy sequence of all possible interleavings of the given
  sequences."
  [op-seqs]
  (let [threads (into {} (map vector (range) op-seqs))
        branch? (comp not-empty :threads)]
    (->>
      {:ordering [], :threads threads}
      (tree-seq branch? thread-children)
      (remove branch?)
      (map :ordering))))


(defn check-system-concurrent
  "Uses generative tests to validate the behavior of a system under multiple concurrent
  threads of operations.

  Takes a test message, a no-arg constructor function which will produce a new
  system for testing, and a function which will return an operation generator
  when called with the test context. The remaining options control the behavior
  of the tests:

  - `context-gen` generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `repetitions` number of times to run per generation to ensure repeatability
  - `max-threads` maximum number of threads to run in parallel
  - `on-stop`     side-effecting function to call on the system after testing

  Returns the results of the generative tests."
  [message constructor op-gen
   & {:keys [context-gen init-model iterations repetitions max-threads on-stop]
      :or {context-gen (gen/return {})
           init-model (constantly {})
           iterations 20
           repetitions 10
           max-threads 4}}]
  {:pre [(fn? constructor)]}
  (checking message (chuck/times iterations)
    [context context-gen
     num-threads (gen/choose 2 max-threads)
     op-seqs (-> (gen/frequency [[1 (gen->Wait context)]
                                 [9 (op-gen context)]])
                 (gen/list)
                 (gen/not-empty)
                 (gen/vector num-threads))]
    (println "\nConcurrent check iteration starting...")
    (let [worlds (interleavings op-seqs)]
      (printf "Generated %d worldlines with %d ops across %d threads:\n"
              (count worlds)
              (count (apply concat op-seqs))
              num-threads)
      (run! prn op-seqs)
      (->>
        (range repetitions)
        (map
          (fn run-tests!
            [i]
            (println "Test repetition" (inc i))
            (let [system (constructor)]
              (try
                (let [start (System/nanoTime)
                      thread-results (run-threads! system op-seqs)
                      elapsed (/ (- (System/nanoTime) start) 1000000.0)
                      _ (printf "Ran tests in %.2f ms\n" elapsed)
                      op-results (into {}
                                       (comp cat (map (juxt #(System/identityHashCode (first %))
                                                            second)))
                                       thread-results)
                      world->results (fn [world]
                                       (map (juxt identity #(op-results (System/identityHashCode %)))
                                            world))
                      check-worldline (fn check-worldline
                                        [world]
                                        (let [results (world->results world)]
                                          (binding [ctest/report (constantly nil)]
                                            (when (valid-results? (init-model context) results)
                                              results))))
                      _ (println "Searching for valid linearization...")
                      start (System/nanoTime)
                      valid-world (some identity (pmap check-worldline worlds))
                      elapsed (/ (- (System/nanoTime) start) 1000000.0)]
                  ; Run one last time, either with the valid world or fail on
                  ; the first world.
                  (valid-results?
                    (init-model context)
                    (if valid-world
                      (do (printf "Found valid world in %.2f ms\n" elapsed)
                          valid-world)
                      (let [results (world->results (first worlds))]
                        (printf "Exhausted worldlines after %.2f ms\n%s\n"
                                elapsed (pr-str results))
                        results))))
                (finally
                  (when on-stop
                    (on-stop system)))))))
        (every? true?)))))
