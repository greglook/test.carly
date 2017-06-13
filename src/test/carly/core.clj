(ns test.carly.core
  "Suite of tests to verify that a given system implementation conforms to the
  spec during a sequence of operations performed on it."
  (:require
    [clojure.test :as ctest]
    [clojure.test.check.generators :as gen]
    (test.carly
      [glue :as glue]
      [op :as op]
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
            (list sym args (glue/wrap-report-check body))
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
  the context.

  If `max-concurrency` is 1, there will be a single thread of operations,
  otherwise there will be between 2 and `max-concurrency` threads."
  [context-gen ctx->op-gens max-concurrency]
  (gen/bind
    (gen/tuple
      context-gen
      (if (<= max-concurrency 1)
        (gen/return 1)
        (gen/choose 2 max-concurrency)))
    (fn [[context concurrency]]
      (gen/tuple
        (gen/return context)
        (-> (ctx->op-gens context)
            (gen/one-of)
            (gen/list)
            (gen/not-empty)
            (gen/vector concurrency))))))


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
        ; TODO: ctest/report :test.carly/run-ops
        (printf "Ran %d operations%s in %.2f ms\n"
                (reduce + 0 (map count op-seqs))
                (let [threads (count op-seqs)]
                  (if (= 1 threads)
                    ""
                    (str " across " threads " threads")))
                (/ (- (System/nanoTime) start) 1000000.0))
        (flush)))))


(defn- run-test!
  "Runs a generative test iteration. Returns true if the test iteration passed."
  [constructor on-stop model thread-count op-seqs]
  (let [op-results (run-ops! constructor on-stop op-seqs)
        result (search/search-worldlines
                 thread-count
                 model
                 op-results
                 ctest/report
                 ; TODO: figure out what to use here
                 #_ (partial glue/report-counter ctest/report))]
      (if (:world result)
        (let [message (format "Found valid worldline in %.2f ms after visiting %d worlds"
                              (:elapsed result) (:visited result))]
          (println message)
          ; TODO: option to show valid linearization
          ;(prn (:history valid-world))
          ;(flush)
          (ctest/do-report
            {:type :pass
             :message message
             :expected '(linearizable? history)
             :actual (get-in result [:world :history])})
          true)
        (let [message (format "Exhausted worldlines in %.2f ms after visiting %d worlds"
                              (:elapsed result) (:visited result))]
          ;(println message)
          ;(flush)
          ; No valid world found, report all assertions as-is.
          (ctest/do-report
            {:type :fail
             :message message
             :expected '(linearizable? history)
             :actual op-results})
          false))))


(defn- run-test-loop!
  [repetitions runner-fn op-seqs]
  ; TODO: ctest/report?
  (printf "\nStarting test iteration of %d repetitions with %d ops across %d threads:\n"
          repetitions
          (count (apply concat op-seqs))
          (count op-seqs))
  ; TODO: option to print out input ops
  (loop [i 0
         result nil]
    (if (<= repetitions i)
      ; ideally, count every assertion but rewrite fail/error as pass?
      true ;result
      (do
        (println "\nTest repetition" (inc i))
        ; TODO: desired outcome:
        ; - on success - count every passed report
        (let [results' (runner-fn op-seqs)]
          (if (:result results')
            (recur (inc i) results')
            ; - on failure - count every passed/failed report from search
            ;   - pretty print shrunk input
            false))))))


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
  (glue/check-and-report
    message iteration-opts
    (gen-test-inputs
      context-gen
      (cond-> ctx->op-gens
        (< 1 concurrency) (waitable-ops))
      concurrency)
    (fn [ctx op-seqs]
      (let [model (init-model ctx)]
        (run-test-loop!
          repetitions
          (partial run-test! init-system (:on-stop opts)
                   model search-threads)
          op-seqs)))))
