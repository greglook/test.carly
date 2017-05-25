(ns test.carly.op
  "Test operations and related functionality.")


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


(defn apply-ops!
  "Apply a sequence of operations to a system, returning a vector of pairs of
  the operations with their results."
  [system ops]
  (mapv #(assoc % ::result (apply-op % system)) ops))


(defn run-ops!
  "Applies a sequence of operations in a separate thread. Returns a promise for
  the results of the application."
  [latch system thread-id ops]
  #_
  (let [results (promise)]
    (doto (Thread.
            (fn apply-ops-thread []
              @latch
              (deliver results (apply-ops! system ops)))
            (format "test.carly/%s/%d" (Integer/toHexString (hash system)) thread-id))
      (.start))
    results)
  (future @latch (apply-ops! system ops)))


(defn run-threads!
  "Run each of the given operation sequences in a separate thread. Returns a
  vector of the operation results for each thread."
  [system op-seqs]
  (let [latch (promise)
        threads (map (partial run-ops! latch system) (range) op-seqs)]
    (dorun threads)
    (deliver latch :start)
    ; TODO: timeout on deref?
    (->> (map deref threads)
         (map vector (range))
         (into {}))))
