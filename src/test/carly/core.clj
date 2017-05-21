(ns test.carly.core
  "Suite of tests to verify that a given system implementation conforms to the
  spec during a sequence of operations performed on it."
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.generators :as gen]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]))


(defprotocol TestOperation
  "Protocol for a test operation on a system."

  (apply-op
    [operation system]
    "Apply the operation to the system, returning a result value.")

  (update-model
    [operation model]
    "Apply an update to the model based on the operation.")

  (check
    [operation model result]
    "Validate an operation given the model state and the response from the
    system being tested. May include `clojure.test/is` assertions, and should
    return a boolean value indicating overall success or failure."))


(defmacro defop
  "Defines a new specification for a system operation test."
  [op-name attr-vec & forms]
  (let [defined (zipmap (map first forms) forms)]
    `(do
       (defrecord ~op-name
         ~attr-vec

         TestOperation

         ~(or (defined 'apply-op) '(apply-op [op system] nil))
         ~(or (defined 'update-model) '(update-model [op model] model))
         ~(or (defined 'check) '(check [op model result] true)))

       (defn ~(symbol (str "gen-" (name op-name) "-op"))
         ~(str "Constructs a " (name op-name) " operation generator.")
         ~@(if-let [[_ args & body] (defined 'generate)]
             [args
              `(gen/fmap
                 ~(symbol (str "map->" (name op-name)))
                 (do ~@body))]
             [['context]
              `(gen/return (~(symbol (str "->" (name op-name)))))])))))



;; ## Operation Testing

(defn apply-ops!
  "Apply a sequence of operations to a system, returning a vector of pairs of
  the operations with their results."
  [system ops]
  (reduce
    (fn [results op]
      (conj results [op (apply-op op system)]))
    []
    ops))


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


(defn check-system
  "Uses generative tests to validate the behavior of a system. The first
  argument must be a no-arg constructor function which will produce a new
  system for testing. The remaining options control the
  behavior of the tests:

  - `op-gen`      function which can be bound to the context to generate test operations
  - `context-gen` generator for the operation test context
  - `init-model`  function which returns a fresh model when called with the context
  - `iterations`  number of generative tests to perform
  - `on-stop`     side-effecting function to call on the system after testing

  Returns the results of the generative tests."
  [message constructor op-gen
   & {:keys [context-gen init-model iterations on-start on-stop]
      :or {context-gen (gen/return {})
           init-model (constantly {})
           iterations 100}}]
  {:pre [(fn? constructor)]}
  (checking message iterations
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


; TODO: concurrent check
