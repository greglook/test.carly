(ns test.carly.report
  "Integration code for wedding generative `test.check` functions and
  `clojure.test` assertion macros."
  (:require
    [clojure.string :as str]
    [clojure.test :as ctest]
    [puget.color.ansi :as ansi]
    [puget.printer :as puget]))


(defn- env-keyword
  "Looks up an environment variable and returns a keywordized value if it is
  present, or nil if not."
  [env-key default]
  (if-let [env-val(System/getenv env-key)]
    (keyword (str/lower-case env-val))
    default))


(def ^:dynamic *options*
  "Report output options."
  {:style (env-keyword "TEST_CARLY_STYLE" :dots)
   :print-color (not (contains? #{:0 :false :no}
                                (env-keyword "TEST_CARLY_COLOR" true)))
   :puget {}})


; TODO: utility for setting pretty-printer handlers


(defn- colorize
  "Applies ANSI coloring to the given text, if the print-color option is true."
  [text & codes]
  (if (:print-color *options*)
    (apply ansi/sgr text codes)
    text))


(defn- pprint
  "Pretty-print the given value using the dynamic reporting options."
  [x]
  (puget/pprint x (assoc (:puget *options*) :print-color (:print-color *options*))))


(defn- format-duration
  "Apply common formatting to elapsed times."
  [elapsed]
  (colorize (format "%.2f ms" elapsed) :cyan))



;; ## Report Methods

;; Report the beginning of a new test trial. Check forms produce one trial per
;; generative iteration, so there may be more if shrinking is necessary.
(defmethod ctest/report ::trial-start
  [result]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "\n%s Starting %s trial%s with %s operations%s %s\n"
                (colorize "<<<" :bold :blue)
                (colorize "test.carly" :magenta)
                (if (< 1 (:repetitions result))
                  (colorize (str " x" (:repetitions result)) :cyan)
                  "")
                (colorize (:op-count result) :bold :yellow)
                (if (< 1 (:concurrency result))
                  (str " across " (colorize (:concurrency result) :cyan) " threads")
                  "")
                (colorize ">>>" :bold :blue))
      :terse
        (do (printf "%12s/%-10s | "
                    (colorize (:op-count result) :cyan)
                    (colorize (:concurrency result) :bold :yellow))
            (flush))
      ; otherwise silent
      nil)))


;; Start of a test repetition. This is mainly useful as a marker.
(defmethod ctest/report ::test-start
  [result]
  (ctest/with-test-out
    (ctest/inc-report-counter :test)
    (case (:style *options*)
      :verbose
        (println "  Starting test repetition...")
      ; otherwise silent
      nil)))


;; Elapsed time to run the operations against a real system.
(defmethod ctest/report ::run-ops
  [result]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "    Ran ops in %s\n" (format-duration (:elapsed result)))
      ; otherwise silent
      nil)))


(defn- report-assertions
  "Common reporting code for assertions at the end of a test."
  [result]
  (let [assertions (frequencies (map :type (:reports result)))
        total (reduce + 0 (vals assertions))]
    ; Record every assertion as a :pass, since failures during searches are not
    ; real failures.
    (when ctest/*report-counters*
      (dosync
        (commute ctest/*report-counters* update :pass (fnil + 0) total)))
    ; Print out assertion counts
    (case (:style *options*)
      :verbose
        (printf "    Checked %s assertions (%d passed, %d failed, %d errors)\n"
                (colorize total :cyan)
                (:pass assertions 0)
                (:fail assertions 0)
                (:error assertions 0))
      ; Otherwise no-op
      nil)))


;; Report that a test repetition passed successfully, indicating that a valid
;; worldline was found.
(defmethod ctest/report ::test-pass
  [result]
  ; TODO: option to show valid linearization
  (ctest/with-test-out
    (ctest/inc-report-counter :pass)
    (report-assertions result)
    (case (:style *options*)
      :verbose
        (printf "    Found valid worldline among %s futures in %s after visiting %s worlds.\n"
                (colorize (:futures result) :cyan)
                (format-duration (:elapsed result))
                (colorize (:visited result) :cyan))
      (:terse :dots)
        (do (print (colorize "." :green))
            (flush))
      ; Otherwise no-op
      nil)))


;; Report that a test repetition failed, indicating that no valid worldline
;; could be found for the observed operation results.
(defmethod ctest/report ::test-fail
  [result]
  (ctest/with-test-out
    (ctest/inc-report-counter :fail)
    (report-assertions result)
    (case (:style *options*)
      :verbose
        (printf "    Exhausted valid worldlines among %s futures in %s after visiting %s worlds!\n"
                (colorize (:futures result) :cyan)
                (format-duration (:elapsed result))
                (colorize (:visited result) :cyan))
      (:terse :dots)
        (do (print (colorize "." :red))
            (flush))
      ; Otherwise no-op
      nil)))


;; An entire test trial passed, meaning every repetition was successful.
(defmethod ctest/report ::trial-pass
  [result]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "Trial %s in %s\n"
                (colorize "PASSED" :bold :green)
                (format-duration (:elapsed result)))
      :terse
        (do (print (colorize "✓" :bold :green)
                   " "
                   (format-duration (:elapsed result)))
            (newline)
            (flush))
      ; Otherwise no-op
      nil)))


;; An entire trial failed, indicating that one or more repetitions was
;; unsuccessful.
(defmethod ctest/report ::trial-fail
  [result]
  (ctest/with-test-out
    (case (:style *options*)
      :verbose
        (printf "Trial %s in %s after %s tests\n"
                (colorize "FAILED" :bold :red)
                (format-duration (:elapsed result))
                (colorize (:repetition result) :cyan))
      :terse
        (do (print (colorize "X" :bold :red))
            (newline)
            (flush))
      ; Otherwise no-op
      nil)))


;; Report a successful generative test summary.
(defmethod ctest/report ::summary
  [summary]
  (ctest/with-test-out
    ; TODO: summarize total assertion counts if possible
    (printf "\nGenerative tests passed after %s trials with seed %s\n"
            (colorize (:num-tests summary) :cyan)
            (colorize (:seed summary) :green))))


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
                                      :smallest (:args smallest)}
                             :shrunk-result {:world nil
                                             :threads n
                                             :futures n
                                             :visited n
                                             :reports [,,,]
                                             :elapsed ms}}
  (ctest/with-test-out
    (newline)
    (printf "Tests failed with seed %s\n"
            (colorize (:seed summary) :red))
    (when-let [shrunk (:shrunk summary)]
      (printf "Shrank inputs %s steps after searching %s nodes\n"
              (colorize (:depth shrunk) :cyan)
              (colorize (:total-nodes-visited shrunk) :cyan)))
    (when-let [[context op-seqs] (get-in summary
                                         [:shrunk :smallest]
                                         (:fail summary))]
      (newline)
      (println "Context:")
      (pprint context)
      (newline)
      (println "Operation sequences:")
      (doseq [ops op-seqs]
        (pprint ops)))
    (newline)
    (println "Result:")
    (let [result (get-in summary [:shrunk :result] (:result summary))]
      (if (instance? Throwable result)
        (clojure.stacktrace/print-cause-trace result)
        (pprint result)))))



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
