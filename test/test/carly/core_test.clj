(ns test.carly.core-test
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

  (generate
    [context]
    (gen/hash-map :k (gen/elements (:keys context))))

  (apply-op
    [this system]
    (get @system k))

  (check
    [this model result]
    (is (= (get model k) result))))


(defop PutEntry
  [k v]

  (generate
    [context]
    (gen/hash-map
      :k (gen/elements (:keys context))
      :v gen/any-printable))

  (apply-op
    [this system]
    (swap! system assoc k v)
    v)

  (update-model
    [this model]
    (assoc model k v))

  (check
    [this model result]
    (is (= v result))))


(defop RemoveEntry
  [k]

  (generate
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
  [(gen-NoOp-op context)
   (gen-ListKeys-op context)
   (gen-GetEntry-op context)
   (gen-PutEntry-op context)
   (gen-RemoveEntry-op context)])


(deftest store-test
  (carly/check-system
    "basic store tests"
    #(atom {})
    (comp gen/one-of op-generators)
    :context-gen (gen/hash-map :keys (gen/set gen/keyword {:min-elements 1}))
    :iterations 10))
