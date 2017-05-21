(defproject mvxcvi/test.carly "0.1.0-SNAPSHOT"
  :description "Generative test harness for stateful system behavior."
  :url "https://github.com/greglook/test.carly"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [org.clojure/test.check "0.9.0"]
   [com.gfredericks/test.chuck "0.2.7"]]

  :test-selectors
  {:default (complement :concurrent)
   :concurrent :concurrent})
