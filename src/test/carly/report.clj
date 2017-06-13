(ns test.carly.report
  "Integration code for wedding generative `test.check` functions and
  `clojure.test` assertion macros."
  (:require
    [clojure.test :as ctest]
    [puget.color.ansi :as ansi]
    [puget.printer :as puget]))


; TODO: output options
(def ^:dynamic *options*
  {:style :verbose
   :print-color true})


; TODO: affordance for setting pretty-printer handlers


(defn- colorize
  "Applies ANSI coloring to the given text, if the print-color option is true."
  [text & codes]
  (if (:print-color *options*)
    (apply ansi/sgr text codes)
    text))


(defn- format-duration
  "Apply common formatting to elapsed times."
  [elapsed]
  (colorize (format "%.2f ms" elapsed) :cyan))



;; ## Report Methods

(defmethod ctest/report ::trial-start
  [report]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "\n%s Starting %s trial%s with %s operations%s %s\n"
                (colorize "<<<" :bold :blue)
                (colorize "test.carly" :magenta)
                (if (< 1 (:repetitions report))
                  (colorize (str " x" (:repetitions report)) :cyan)
                  "")
                (colorize (:op-count report) :bold :yellow)
                (if (< 1 (:concurrency report))
                  (str " across " (colorize (:concurrency report) :cyan) " threads")
                  "")
                (colorize ">>>" :bold :blue))
      ; otherwise silent
      nil)))


(defmethod ctest/report ::test-start
  [report]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (println "  Starting test repetition...")
      ; otherwise silent
      nil)))


(defmethod ctest/report ::run-ops
  [report]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "    Ran %s ops%s in %s\n"
                (colorize (:op-count report) :bold :yellow)
                (if (< 1 (:concurrency report))
                  (str " across " (colorize (:concurrency report) :cyan) " threads")
                  "")
                (format-duration (:elapsed report)))
      ; otherwise silent
      nil)))


#_ ; search returns:
{:world valid-world
 :threads 1
 :futures n
 :visited @visited
 :reports @reports
 :elapsed elapsed}


; TODO: update nested assertion pass/fail/error counts?
(defmethod ctest/report ::test-pass
  [report]
  ; TODO: option to show valid linearization
  (ctest/with-test-out
    (ctest/inc-report-counter :pass)
    (case (:style *options*)
      :verbose
        (printf "    Found valid worldline among %s futures in %s after visiting %s worlds.\n"
                (colorize (:futures report) :cyan)
                (format-duration (:elapsed report))
                (colorize (:visited report) :cyan))
      ,,,)))


(defmethod ctest/report ::test-fail
  [report]
  (ctest/with-test-out
    (ctest/inc-report-counter :fail)
    (case (:style *options*)
      :verbose
        (printf "    Exhausted valid worldlines among %s futures in %s after visiting %s worlds!\n"
                (colorize (:futures report) :cyan)
                (format-duration (:elapsed report))
                (colorize (:visited report) :cyan))
      ,,,)))


(defmethod ctest/report ::trial-pass
  [report]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "Trial %s in %s checking %s assertions (%d passed, %d failed, %d errors)\n"
                (colorize "PASSED" :bold :green)
                (format-duration (:elapsed report))
                (colorize (reduce + 0 (vals (:assertions report))) :cyan)
                (get-in report [:assertions :pass] 0)
                (get-in report [:assertions :fail] 0)
                (get-in report [:assertions :error] 0))
      ; TODO: green check mark at end of dots?
      ,,,)))


(defmethod ctest/report ::trial-fail
  [report]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "Trial %s in %s checking %d assertions (%d passed, %d failed, %d errors)\n"
                (colorize "FAILED" :bold :red)
                (format-duration (:elapsed report))
                (colorize (reduce + 0 (vals (:assertions report))) :cyan)
                (get-in report [:assertions :pass] 0)
                (get-in report [:assertions :fail] 0)
                (get-in report [:assertions :error] 0))
      ; TODO: red X at end of dots?
      ,,,)))


;; Report a successful generative test summary.
(defmethod ctest/report ::summary
  [summary]
                            #_ ; success
                            {:result truthy
                             :num-tests num-trials
                             :seed seed}
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (do
          (newline)
          (printf "Generative tests passed with seed %s:\n" (:seed summary))
          ; TODO: better formatting, include captured assertion totals
          (prn summary))
      ,,,)))


;; Report the shrunk value of a failed test summary.
(defmethod ctest/report ::shrunk
  [summary]
                            #_ ; fail
                            {:result false-or-exception
                             :seed seed
                             :failing-size size
                             :num-tests (inc trial-number)
                             :fail (vec failing-args)
                             :shrunk {:total-nodes-visited total-nodes-visited
                                      :depth depth
                                      :result (:result smallest)
                                      :smallest (:args smallest)}}
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (do
          (newline)
          (printf "Tests failed with seed %s - smallest case:\n" (:seed summary))
          ; TODO: better formatting
          (prn (dissoc (:shrunk summary) :smallest))
          (let [[context op-seqs] (get-in summary [:shrunk :smallest])]
            (newline)
            (println "Context:")
            (puget/cprint context)
            (newline)
            (println "Operation sequences:")
            (doseq [ops op-seqs]
              (puget/cprint ops)))
          (newline)
          (println "Result:")
          (let [result (get-in summary [:shrunk :result])]
            (if (instance? Throwable result)
              (clojure.stacktrace/print-cause-trace result)
              (puget/cprint result))))
      ,,,)))



;; ## Helper Functions

(defmacro capture-reports
  "Capture any clojure.test reports generated by the body. Returns a vector
  containing the evaluated result, followed by a vector of reports."
  [& body]
  `(let [reports# (atom [])]
     [(binding [ctest/report (partial swap! reports# conj)]
        ~@body)
      @reports#]))


(defn publish!
  "Publish a collection of reports to clojure.test."
  [reports]
  (run! ctest/report reports))


(defn wrap-report-check
  "Wrap the given sequence of forms such that it returns false if there is a
  failed clojure.test assertion in the body. If there are no assertions,
  returns the result of evaluating the body."
  [body]
  `(let [[result# reports#] (capture-reports ~@body)]
     (publish! reports#)
     (if (empty? reports#)
       result#
       (not (some (comp #{:fail :error} :type) reports#)))))
