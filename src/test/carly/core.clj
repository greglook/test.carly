(ns test.carly.core
  "Suite of tests to verify that a given system implementation conforms to the
  spec during a sequence of operations performed on it."
  (:require
    [clojure.test :as ctest]
    [clojure.test.check.generators :as gen]
    [com.gfredericks.test.chuck :as chuck]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    (test.carly
      [clojure-test :as tcct]
      [op :as op]
      [search :as search]
      [world :as world])))


;; ## Test Operations

(defn- wrap-op-check
  "Wrap the body of an operation's 'check' method such that it returns true if
  every test assertion in the body passes."
  [[sym args & body]]
  (list
    sym args
    `(let [reports# (tcct/capture-reports ~@body)]
       (tcct/publish! reports#)
       (not (some (comp #{:fail :error} :type) reports#)))))


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

         ~(if-let [form (defined 'check)]
            (wrap-op-check form)
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



;; ## Test Harnesses

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
        ; TODO: ctest/report type?
        (printf "Ran %d operations%s in %.2f ms\n"
                (reduce + 0 (map count op-seqs))
                (let [threads (count op-seqs)]
                  (if (= 1 threads)
                    ""
                    (str " across " threads " threads")))
                (/ (- (System/nanoTime) start) 1000000.0))
        (flush)))))


(defn- gen-test-inputs
  "Create a generator for inputs to a system under test. This generator
  produces a context and a collection of sequences of operations generated from
  the context.

  If `max-concurrency` is 1, there will be a single thread of operations,
  otherwise there will be between 2 and `max-concurrency` threads."
  [context-gen op-generators max-concurrency]
  (gen/bind
    (gen/tuple
      (or context-gen (gen/return {}))
      (if (<= max-concurrency 1)
        (gen/return 1)
        (gen/choose 2 max-concurrency)))
    (fn [[context concurrency]]
      (gen/tuple
        (gen/return context)
        (-> (op-generators context)
            (gen/one-of)
            (gen/list)
            (gen/not-empty)
            (gen/vector concurrency))))))


(defn- run-concurrent-test!
  "Runs a concurrent test iteration. Returns true if the test iteration passed."
  [constructor on-stop model thread-count op-seqs]
  (let [op-results (run-ops! constructor on-stop op-seqs)
        result (search/find-valid-worldline
                 thread-count
                 model
                 op-results
                 (partial tcct/report-counter ctest/report))]
      (if (:world result)
        (let [message (format "Found valid worldline in %.2f ms after visiting %d worlds"
                              (:elapsed result) (:visited result))]
          (println message)
          ; TODO: option to show valid linearization
          ;(prn (:history valid-world))
          (flush)
          (ctest/do-report
            {:type :pass
             :message message
             :expected 'linearizable
             :actual (get-in result [:world :history])})
          true)
        (let [message (format "Exhausted worldlines in %.2f ms after visiting %d worlds"
                              (:elapsed result) (:visited result))]
          (println message)
          (flush)
          ; No valid world found, report all assertions as-is.
          (ctest/do-report
            {:type :fail
             :message message
             :expected 'linearizable?
             :actual op-results})
          false))))


(defn check-system
  "Uses generative tests to validate the behavior of a system under a linear
  sequence of operations.

  Takes a test message, a no-arg constructor function which will produce a new
  system for testing, and a function which will return a vector of operation
  generators when called with the test context. The remaining options control
  the behavior of the tests:

  - `context`     generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `on-stop`     side-effecting function to call on the system after testing"
  [message constructor op-generators
   & {:keys [context init-model iterations on-stop]
      :or {context (gen/return {})
           init-model (constantly {})
           iterations 100}}]
  {:pre [(fn? constructor)]}
  (tcct/check-and-report
    message iterations
    (prop/for-all
      [[ctx op-seqs] (gen-test-inputs context op-generators 1)]
      (let [results (run-ops! constructor on-stop op-seqs)]
        ; No special capturing here.
        (world/run-linear
          (world/initialize (init-model ctx) results)
          (constantly nil))))))


(defn check-system-concurrent
  "Uses generative tests to validate the behavior of a system under multiple concurrent
  threads of operations.

  Takes a test message, a no-arg constructor function which will produce a new
  system for testing, and a function which will return a vector of operation
  generators when called with the test context. The remaining options control
  the behavior of the tests:

  - `context`     generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `repetitions` number of times to run per generation to ensure repeatability
  - `max-concurrency` maximum number of operation threads to run in parallel
  - `search-threads` number of threads to run to search for valid worldlines
  - `on-stop`     side-effecting function to call on the system after testing

  Returns the results of the generative tests."
  [message constructor op-generators
   & {:keys [context init-model iterations repetitions max-concurrency search-threads on-stop]
      :or {context (gen/return {})
           init-model (constantly {})
           iterations 20
           repetitions 10
           max-concurrency 4
           search-threads (. (Runtime/getRuntime) availableProcessors)}}]
  {:pre [(fn? constructor)]}
  (tcct/check-and-report
    message iterations
    (prop/for-all
      [[ctx op-seqs] (gen-test-inputs context (waitable-ops op-generators) max-concurrency)]
      ; TODO: ctest/report?
      (printf "\nStarting test iteration of %d repetitions with %d ops across %d threads:\n"
              repetitions
              (count (apply concat op-seqs))
              (count op-seqs))
      ; TODO: option to print out input ops
      (loop [i 0]
        (if (<= repetitions i)
          ; ideally, count every assertion but rewrite fail/error as pass?
          true
          (do
            (println "\nTest repetition" (inc i))
            ; TODO: desired outcome:
            ; - on success - count every passed report
            (if (run-concurrent-test! constructor on-stop (init-model ctx) search-threads op-seqs)
              (recur (inc i))
              ; - on failure - count every passed/failed report from search
              ;   - pretty print shrunk input
              false)))))))
