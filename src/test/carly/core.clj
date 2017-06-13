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


(defn- wrap-op-check
  "Wrap the body of an operation's 'check' method such that it returns true if
  every test assertion in the body passes."
  [[sym args & body]]
  (list
    sym args
    `(let [reports# (tcct/capture-reports ~@body)]
       (tcct/publish! reports#)
       (every? (comp #{:pass} :type) reports#))))


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



;; ## Test Harnesses

(defn- run-ops!
  "Run a collection of op sequences on the system in parallel. Returns a map
  from thread index to operations updated with results."
  [system op-seqs]
  (let [start (System/nanoTime)
        results (op/run-threads! system op-seqs)
        elapsed (/ (- (System/nanoTime) start) 1000000.0)]
    (printf "Ran %d operations%s in %.2f ms\n"
            (reduce + 0 (map count op-seqs))
            (let [threads (count op-seqs)]
              (if (= 1 threads)
                ""
                (str " across " threads " threads")))
            elapsed)
    (flush)
    results))


(defn- run-test!
  "Runs a concurrent test iteration. Returns ???"
  [constructor init-model context op-seqs thread-count on-stop]
  (let [system (constructor)]
    (try
      (let [op-results (run-ops! system op-seqs)
            ; TODO: report-fn needs to capture test results, and if a valid world is found
            ; they should actually all be 'pass' or maybe 'info' instead of fail. If a valid
            ; world is not found, they should be reported as failures directly.
            report-fn (constantly nil)
            result (search/find-valid-worldline thread-count (init-model context) op-results report-fn)]
          (if (:world result)
            (let [message (format "Found valid worldline in %.2f ms after visiting %d worlds"
                                  (:elapsed result) (:visited result))]
              (println message)
              ;(prn (:history valid-world))
              (flush)
              ; Found a valid world, so :fail results should be converted into :pass or :info?
              ; ???
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
                 :expected 'linearizable
                 :actual op-results})
              false)))
      (finally
        (when on-stop
          (on-stop system))))))


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
  (checking message (chuck/times iterations)
    [context context
     ops (gen/not-empty (gen/list (gen/one-of (op-generators context))))]
    (let [system (constructor)]
      (try
        (let [results (op/apply-ops! system ops)]
          (world/run-linear
            (world/initialize (init-model context) {0 results})
            (constantly nil)))
        (finally
          (when on-stop
            (on-stop system)))))))


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
  (checking message (chuck/times iterations)
    [context context
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
    ; TODO: option to print out input ops
    (loop [i 0]
      (if (<= repetitions i)
        true
        (do
          (println "\nTest repetition" (inc i))
          (if (run-test! constructor init-model context op-seqs search-threads on-stop)
            (recur (inc i))
            false))))))
