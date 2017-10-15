(defproject mvxcvi/test.carly "0.4.0"
  :description "Generative test harness for stateful system behavior."
  :url "https://github.com/greglook/test.carly"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [org.clojure/test.check "0.9.0"]
   [mvxcvi/puget "1.0.1"]]

  :test-selectors
  {:default (complement :concurrent)
   :concurrent :concurrent}

  :hiera
  {:cluster-depth 2
   :vertical false
   :show-external true
   :ignore-ns #{}}

  :codox
  {:metadata {:doc/format :markdown}
   :source-uri "https://github.com/greglook/test.carly/blob/master/{filepath}#L{line}"
   :output-path "target/doc/api"})
