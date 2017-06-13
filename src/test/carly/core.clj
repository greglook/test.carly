(ns test.carly.core
  "Suite of tests to verify that a given system implementation conforms to the
  spec during a sequence of operations performed on it."
  (:require
    [clojure.test :as ctest]
    [clojure.test.check.generators :as gen]
    (test.carly
      [check :as check]
      [op :as op]
      [report :as report]
      [search :as search])))


;; ## Test Operation Definition

(defn- generator-body
  "Macro helper to build a generator constructor."
  [op-name [form :as body]]
  (cond
    (and (= 1 (count body)) (vector? form))
      `(gen/fmap
         (partial apply ~(symbol (str "->" (name op-name))))
         (gen/tuple ~@form))
    (and (= 1 (count body)) (map? form))
      `(gen/fmap
         ~(symbol (str "map->" (name op-name)))
         (gen/hash-map ~@(apply concat form)))
    :else
      `(gen/fmap
         ~(symbol (str "map->" (name op-name)))
         (do ~@body))))


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

         ~(or (defined 'apply-op)
              '(apply-op [op system] nil))

         ~(if-let [[sym args & body] (defined 'check)]
            (list sym args (report/wrap-report-check body))
            '(check [op model result] true))

         ~(or (defined 'update-model)
              '(update-model [op model] model)))

       (defn ~(symbol (str "gen->" (name op-name)))
         ~(str "Constructs a " (name op-name) " operation generator.")
         ~@(if-let [[_ args & body] (defined 'gen-args)]
             [args (generator-body op-name body)]
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


(defn- waitable-ops
  "Takes a function from context to vector of op generators and returns a
  new function which additionally returns the wait op as the first result"
  [op-generators]
  (comp (partial cons (gen->Wait nil)) op-generators))



;; ## Test Harness

(defn- gen-test-inputs
  "Create a generator for inputs to a system under test. This generator
  produces a context and a collection of sequences of operations generated from
  the context."
  [context-gen ctx->op-gens max-concurrency]
  (gen/bind
    (gen/tuple
      context-gen
      (if (<= max-concurrency 1)
        (gen/return 1)
        (gen/choose 1 max-concurrency)))
    (fn [[context concurrency]]
      (gen/tuple
        (gen/return context)
        (-> (ctx->op-gens context)
            (gen/one-of)
            (gen/list)
            (gen/not-empty)
            (gen/vector concurrency))))))


(defn- elapsed-since
  "Returns the number of milliseconds elapsed since an initial start time
  in system nanoseconds."
  [start]
  (/ (- (System/nanoTime) start) 1000000.0))


(defn- run-ops!
  "Construct a system, run a collection of op sequences on the system (possibly
  concurrently), and shut the system down. Returns a map from thread index to
  operations updated with results."
  [constructor on-stop op-seqs]
  (let [start (System/nanoTime)
        system (constructor)]
    (try
      (case (count op-seqs)
        0 op-seqs
        1 {0 (op/apply-ops! system (first op-seqs))}
          (op/run-threads! system op-seqs))
      (finally
        (when on-stop
          (on-stop system))
        (ctest/do-report
          {:type ::report/run-ops
           :op-count (reduce + 0 (map count op-seqs))
           :concurrency (count op-seqs)
           :elapsed (elapsed-since start)})))))


(defn- run-test!
  "Runs a generative test iteration. Returns true if the test iteration passed."
  [constructor on-stop model thread-count op-seqs]
  (ctest/do-report
    {:type ::report/test-start})
  (let [op-results (run-ops! constructor on-stop op-seqs)
        result (search/search-worldlines thread-count model op-results)]
    (ctest/do-report
      (assoc result :type (if (:world result)
                            ::report/test-pass
                            ::report/test-fail)))
    result))


(defn- run-trial!
  "Run a generative trial involving multiple test repetitions."
  [repetitions runner-fn op-seqs]
  (let [start (System/nanoTime)]
    (ctest/do-report
      {:type ::report/trial-start
       :repetitions repetitions
       :concurrency (count op-seqs)
       :op-count (reduce + 0 (map count op-seqs))})
    (loop [i 0
           result nil]
      (if (== repetitions i)
        (do (ctest/do-report
              ; TODO: send result counts?
              {:type ::report/trial-pass
               :repetitions repetitions
               :elapsed (elapsed-since start)})
            ; ideally, count every assertion but rewrite fail/error as pass?
            result)
        (let [result' (runner-fn op-seqs)]
          (if (:world result')
            (recur (inc i) result')
            ; - on failure - count every passed/failed report from search
            (do (ctest/do-report
                  {:type ::report/trial-fail
                   :repetition (inc i)
                   :elapsed (elapsed-since start)})
                result')))))))


(defn- report-test-summary
  "Emit clojure.test reports for the summarized results of the generative
  tests."
  [summary]
  (if (and (:result summary) (not (instance? Throwable (:result summary))))
    (ctest/report
      (assoc summary :type ::report/summary))
    (ctest/report
      (assoc summary
             :type ::shrunk
             :shrunk-result (::check/result (meta (get-in summary [:shrunk :smallest])))))))


(defn check-system
  "Uses generative tests to validate the behavior of a system under a linear
  sequence of operations.

  Takes a test message, a no-arg constructor function which will produce a new
  system for testing, and a function which will return a vector of operation
  generators when called with the test context. The remaining options control
  the behavior of the tests:

  - `on-stop`         side-effecting function to call on the system after testing
  - `context-gen`     generator for the operation test context
  - `init-model`      function which returns a fresh model when called with the context
  - `concurrency`     maximum number of operation threads to run in parallel
  - `repetitions`     number of times to run per generation to ensure repeatability
  - `search-threads`  number of threads to run to search for valid worldlines"
  ; TODO: verbose option
  [message
   iteration-opts
   init-system
   ctx->op-gens
   & {:keys [context-gen init-model concurrency repetitions search-threads]
      :or {context-gen (gen/return {})
           init-model (constantly {})
           concurrency 4
           repetitions 5
           search-threads (. (Runtime/getRuntime) availableProcessors)}
      :as opts}]
  {:pre [(fn? init-system) (fn? ctx->op-gens)]}
  (ctest/testing message
    (report-test-summary
      (check/check-and-report
        iteration-opts
        (gen-test-inputs
          context-gen
          (cond-> ctx->op-gens
            (< 1 concurrency) (waitable-ops))
          concurrency)
        (fn [ctx op-seqs]
          (let [model (init-model ctx)]
            (run-trial!
              repetitions
              (partial run-test! init-system (:on-stop opts) model search-threads)
              op-seqs)))))))
