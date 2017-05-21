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
     :v gen/any-printable})

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


(deftest store-test
  (carly/check-system
    "basic store tests"
    #(atom {})
    (comp gen/one-of op-generators)
    :context-gen (gen/hash-map :keys (gen/set gen/keyword {:min-elements 1}))
    :iterations 10))
