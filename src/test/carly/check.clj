(ns test.carly.check
  "Integration code for wedding generative `test.check` functions and
  `clojure.test` assertion macros."
  (:require
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :as tcct]
    [clojure.test.check.generators :as gen]))


(defn- apply-test
  "Helper to produce a property by applying the test function to a realized
  set of arguments from some bound generator. The function should accept a
  context map and a collection of op sequences, and return a result map with a
  `:world` entry containing a valid terminal world on success, or nil on
  failure."
  [function]
  (fn [args]
    (try
      (let [result (apply function args)]
        {:result (boolean (:world result))
         :function function
         ; XXX: Super gross, but we need to do this to get result metadata
         ; through for failing results until test.check 0.10.0. ಠ_ಠ
         :args (vary-meta args assoc ::result result)})
      (catch ThreadDeath td
        (throw td))
      (catch Throwable ex
        {:result ex
         :function function
         :args args}))))


(defn check-and-report
  "Apply generative tests to the given input generator and testing function,
  returning a test.check summary."
  [opts gen-inputs f]
  (let [opts (tcct/process-options opts)]
    (apply tc/quick-check
      (:num-tests opts 20)
      (gen/fmap (apply-test f) gen-inputs)
      (apply concat (dissoc opts :num-tests)))))
