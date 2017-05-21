(ns test.carly.example-test
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.generators :as gen]
    [test.carly.core :as carly :refer [defop]]))


(defop NoOp [])


(defop ListKeys
  []

  (apply-op
    [this system]
    (keys @system))

  (check
    [this model result]
    (is (= (keys model) result))))


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


(defn op-generators
  [context]
  [(gen->NoOp context)
   (gen->ListKeys context)
   (gen->GetEntry context)
   (gen->PutEntry context)
   (gen->RemoveEntry context)])


(def gen-context
  "Generator for test contexts; this gives the set of possible keys to use in
  operations."
  (gen/hash-map :keys (gen/set (gen/fmap (comp keyword str) gen/char-alpha) {:min-elements 1})))


(deftest store-test
  (carly/check-system
    "basic store tests"
    #(atom (sorted-map))
    op-generators
    :context-gen gen-context
    :iterations 20))


(deftest ^:concurrent concurrent-test
  (carly/check-system-concurrent
    "concurrent store tests"
    #(atom (sorted-map))
    op-generators
    :context-gen gen-context
    :iterations 10
    :repetitions 5))
