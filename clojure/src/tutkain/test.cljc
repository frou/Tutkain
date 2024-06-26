(ns tutkain.test
  (:require
   [clojure.stacktrace :as stacktrace]
   [clojure.string :as str]
   [clojure.test :as test]
   [clojure.walk :as walk]
   [tutkain.format :refer [pp-str Throwable->str]]
   [tutkain.base64 :refer [load-base64]]
   [tutkain.rpc :refer [handle respond-to]])
  (:import
   (java.io File)))

(defn organize
  [x]
  (walk/postwalk
    #(cond
       (map? %) (try
                  (into (sorted-map) %)
                  (catch ClassCastException _
                    %))
       :else %)
    x))

(defn- pprint-expected
  [{:keys [actual expected] :as event}]
  (if (and (= (first expected) '=) (sequential? actual))
    (assoc event :expected (->> actual last second organize pp-str))
    (update event :expected str)))

(defn- pprint-actual
  [{:keys [actual] :as event}]
  (if (sequential? actual)
    (update event :actual (comp pp-str organize last last))
    (update event :actual pp-str)))

(defn- event-var-meta
  [event]
  (-> event :var meta (select-keys [:line :column :file :name :ns]) (update :ns str)))

(defn- class-name-prefix
  [ns]
  (str/replace (format "%s$" ns) "-" "_"))

(defn- line-number
  [ns]
  (or
    (when-some [^StackTraceElement stack-trace-element
                (some->>
                  (.getStackTrace (new java.lang.Throwable))
                  (drop-while #(not (str/starts-with? (.getClassName ^StackTraceElement %)
                                      (class-name-prefix ns))))
                  (first))]
      (.getLineNumber stack-trace-element))
    0))

(defn- add-result
  [results id result]
  (swap! results update id (fnil conj []) result))

(defn ^:private clean-ns!
  [ns]
  (some->> ns ns-aliases (run! #(ns-unalias ns (first %))))
  (some->> ns ns-publics
    (filter (fn [[_ v]] (-> v meta :test)))
    (run! (fn [[sym _]] (ns-unmap ns sym)))))

(defn run-tests
  [ns file vars]
  (let [results (atom {:fail [] :pass [] :error []})
        var-meta (atom nil)]
    (binding [*print-length* nil
              *print-level* nil
              test/report (fn [event*]
                            (let [{:keys [type] :as event} (cond-> event* (seq file) (assoc :file file))]
                              ;; TODO: Could use a multimethod here instead.
                              (case type
                                :begin-test-var (reset! var-meta (event-var-meta event))
                                :end-test-var (reset! var-meta nil)
                                :fail (do
                                        (test/inc-report-counter :assert)
                                        (test/inc-report-counter :fail)
                                        (add-result results :fail
                                          (-> event
                                            pprint-expected
                                            pprint-actual
                                            (assoc :var-meta @var-meta))))
                                :pass (do
                                        (test/inc-report-counter :assert)
                                        (test/inc-report-counter :pass)
                                        (add-result results :pass
                                          {:type :pass :line (line-number ns) :var-meta @var-meta}))
                                :error (do
                                         (test/inc-report-counter :assert)
                                         (test/inc-report-counter :error)
                                         (add-result results :error
                                           (-> event
                                             pprint-expected
                                             (update :actual #(with-out-str (stacktrace/print-stack-trace %)))
                                             (assoc :var-meta @var-meta))))
                                :summary (swap! results assoc :tag :ret :val (str (-> event (dissoc :file)) \newline))
                                nil)))]
      (if (seq vars)
        (binding [test/*report-counters* (ref test/*initial-report-counters*)]
          (test/test-vars (map #(resolve (symbol (name ns) %)) vars))
          (swap! results assoc :tag :ret :val (str (assoc @test/*report-counters* :type :summary) \newline)))
        (test/run-tests ns)))
    @results))

(defmethod handle :test
  [{:keys [eval-lock ns code ^String file vars thread-bindings] :as message}]
  (try
    (let [filename (some-> file File. .getName)
          ns-sym (or (some-> ns symbol) 'user)]
      (clean-ns! (find-ns ns-sym))
      (locking eval-lock (load-base64 code file filename))
      (respond-to message (run-tests ns-sym file vars)))
    (catch Throwable ex
      (swap! thread-bindings assoc #'*e ex)
      (respond-to message {:tag :err
                           :val (Throwable->str ex)
                           :exception true}))))
