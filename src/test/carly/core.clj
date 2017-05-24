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



;; ## Operation Execution

(defn apply-ops!
  "Apply a sequence of operations to a system, returning a vector of pairs of
  the operations with their results."
  [system ops]
  (mapv #(assoc %1
                ::rank %2
                ::result (apply-op %1 system))
        ops
        (range)))


(defn run-ops!
  "Applies a sequence of operations in a separate thread. Returns a promise for
  the results of the application."
  [latch system thread-id ops]
  (let [results (promise)
        ops (map #(assoc % ::thread thread-id) ops)]
    (doto (Thread.
            (fn apply-ops-thread []
              @latch
              (deliver results (apply-ops! system ops)))
            (format "test.carly/%s/%d" (Integer/toHexString (hash system)) thread-id))
      (.start))
    results))


(defn run-threads!
  "Run each of the given operation sequences in a separate thread. Returns a
  vector of the operation results for each thread."
  [system op-seqs]
  (let [latch (promise)
        threads (map (partial run-ops! latch system) (range) op-seqs)]
    (dorun threads)
    (deliver latch :start)
    ; TODO: timeout on deref?
    (mapv deref threads)))



;; ## World Modeling

;; A world represents a point in time along a possible history. The model
;; holds the current representation of the system, the history is the sequence
;; of operations which have already happened, and pending is a map from thread
;; ids to lists of pending operations for each thread.
(defrecord World
  [model history pending])


(defn- end-of-line?
  "Determine whether the worldline has ended."
  [world]
  (empty? (:pending world)))


(defn- peek-pending
  "Get the next pending operation for the identified thread, if any."
  [pending thread-id]
  (first (get pending thread-id)))


(defn- pop-pending
  "Remove the next pending operation for the identified thread. Returns an
  updated map with the remaining ops, or without the thread if no ops were left."
  [pending thread-id]
  (let [[_ & more] (get pending thread-id)]
    (if (seq more)
      (assoc pending thread-id more)
      (dissoc pending thread-id))))


(defn- future-count
  "Calculate the number of possible futures a world has based on a map of thread
  ids to pending operations."
  [pending]
  (if (<= (count pending) 1)
    1
    (apply + (map (comp future-count (partial pop-pending pending))
                  (keys pending)))))


(defn- step
  "Compute a step by applying the next operation from the identified thread to
  the world. Returns an updated world state, or nil if the operation result was
  invalid."
  [world thread-id]
  (let [op (peek-pending (:pending world) thread-id)]
    (when-not op
      (throw (IllegalStateException.
               (format "Cannot step thread %d - no ops pending" thread-id))))
    (when-not (contains? op ::result)
      (throw (IllegalStateException.
               (format "Cannot step op %s with no result" (pr-str op)))))
    (when (check op (:model world) (::result op))
      (-> world
          (update :model (partial update-model op))
          (update :history conj op)
          (update :pending pop-pending thread-id)))))


(defn- next-steps
  "Compute all possible valid next steps for the world, returning a sequence of
  new world states."
  [world]
  (keep (partial step world) (keys (:pending world))))


(defn- ^:deprecated valid-results?
  "Determines whether the given sequence of operations produced valid results
  when applied to the system. Returns true if the system behavior is valid."
  [model ops]
  (loop [model model
         ops ops]
    (if-let [op (first ops)]
      (if (check op model (::result op))
        (recur (update-model op model) (rest ops))
        false)
      true)))



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
        (let [results (apply-ops! system ops)]
          (loop [world (->World (init-model context) [] {0 results})]
            (if (end-of-line? world)
              ; Made it to the end of the world line with consistent results.
              true
              ; Step world forward. A nil here means the next operation result
              ; is invalid, so the observed worldline is inconsistent with the
              ; model.
              (when-let [world' (step world 0)]
                (recur world')))))
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
  system for testing, and a function which will return a vector of operation
  generators when called with the test context. The remaining options control
  the behavior of the tests:

  - `context-gen` generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `repetitions` number of times to run per generation to ensure repeatability
  - `max-threads` maximum number of threads to run in parallel
  - `on-stop`     side-effecting function to call on the system after testing

  Returns the results of the generative tests."
  [message constructor op-generators
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
     op-seqs (-> (gen->Wait context)
                 (cons (op-generators context))
                 (gen/one-of)
                 (gen/list)
                 (gen/not-empty)
                 (gen/vector num-threads))]
    (println "\nConcurrent check iteration starting...")
    (let [op-seqs (map (fn [op-seq thread-idx]
                         (map #(assoc %1 ::thread thread-idx ::rank %2)
                               op-seq
                               (range)))
                       op-seqs
                       (range))
          worlds (interleavings op-seqs)]
      (printf "Generated %s worldlines with %d ops across %d threads:\n"
              (let [head (count (take 1001 worlds))]
                (if (= 1001 head) "1000+" head))
              (count (apply concat op-seqs))
              num-threads)
      (run! prn op-seqs)
      (loop [i 0]
        (if (<= repetitions i)
          true
          (do
            (println "Test repetition" (inc i))
            (let [system (constructor)]
              (if (try
                    (let [start (System/nanoTime)
                          thread-results (run-threads! system op-seqs)
                          elapsed (/ (- (System/nanoTime) start) 1000000.0)
                          _ (printf "Ran operations in %.2f ms\n" elapsed)
                          op-results (into {}
                                           (comp cat (map (juxt (comp (juxt ::thread ::rank) first) second)))
                                           thread-results)
                          world->results (fn [world]
                                           (map (juxt identity (comp op-results (juxt ::thread ::rank))) world))
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
                      (on-stop system))))
                (recur (inc i))
                false))))))))
