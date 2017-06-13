(ns test.carly.check
  "Integration code for wedding generative `test.check` functions and
  `clojure.test` assertion macros."
  (:require
    [clojure.test :as ctest]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :as tcct]
    [clojure.test.check.generators :as gen]))


(defn- apply-test
  "Helper to produce a property by applying the test function to a realized
  set of arguments from some bound generator."
  [function]
  (fn [args]
    (let [result (try
                   (apply function args)
                   (catch ThreadDeath td
                     (throw td))
                   (catch Throwable ex
                     ex))]
      {:result result
       :function function
       ; XXX: Super gross, but we need to do this to get result metadata
       ; through for failing results until test.check 0.10.0. ಠ_ಠ
       :args (vary-meta args assoc ::result result)})))


(defn check-and-report
  "Apply generative tests to the given input generator and testing function,
  returning a test.check summary."
  [opts gen-inputs f]
  (let [opts (tcct/process-options opts)]
    (apply tc/quick-check
      (:num-tests opts 20)
      (gen/fmap (apply-test f) gen-inputs)
      (apply concat (dissoc opts :num-tests)))))
