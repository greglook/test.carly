(ns test.carly.example-test
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.generators :as gen]
    [test.carly.core :as carly :refer [defop]]))


;; Here we define a no-parameter operation which reads from the system. It does
;; not change the model state, but does check that the system returned the
;; expected results.
(defop ListKeys
  []

  (apply-op
    [this system]
    (keys @system))

  (check
    [this model result]
    (is (= (not-empty (sort (keys model))) result))))


;; This operation specifies a key to lookup in the store, so it defines a
;; `gen-args` form. By returning a vector, the positional generators are used
;; to select a value for each field in the operation.
(defop GetEntry
  [k]

  (gen-args
    [context]
    [(gen/elements (:keys context))])

  (apply-op
    [this system]
    (get @system k))

  (check
    [this model result]
    (is (= (get model k) result))))


;; Put is a side-effecting entry, so it defines an `update-model` method. This
;; returns an updated version of the model state after applying the operation.
;; This op also shows another way to generate args, by specifying a map of field
;; keys to value generators.
(defop PutEntry
  [k v]

  (gen-args
    [context]
    {:k (gen/elements (:keys context))
     :v gen/large-integer})

  (apply-op
    [this system]
    (swap! system assoc k v)
    v)

  (check
    [this model result]
    (is (= v result)))

  (update-model
    [this model]
    (assoc model k v)))


;; Remove is also side-effecting, but does not define any checking logic. It
;; generates a map of args with a full generator expression, which is passed
;; to the record's map constructor.
(defop RemoveEntry
  [k]

  (gen-args
    [context]
    (gen/hash-map :k (gen/elements (:keys context))))

  (apply-op
    [this system]
    (swap! system dissoc k)
    nil)

  (update-model
    [this model]
    (dissoc model k)))


(def op-generators
  "Returns a vector of operation generators when called with the test context."
  (juxt gen->ListKeys
        gen->GetEntry
        gen->PutEntry
        gen->RemoveEntry))


(def gen-context
  "Generator for test contexts; this gives the set of possible keys to use in
  operations."
  (gen/hash-map :keys (gen/set (gen/fmap (comp keyword str) gen/char-alpha) {:min-elements 1})))


(deftest linear-store-test
  (carly/check-system "basic linear store tests" 100
    #(atom (sorted-map))
    op-generators
    :context-gen gen-context
    :concurrency 1
    :repetitions 1))


(deftest ^:concurrent concurrent-store-test
  (carly/check-system "concurrent store tests" 100
    #(atom (sorted-map))
    op-generators
    :context-gen gen-context))
