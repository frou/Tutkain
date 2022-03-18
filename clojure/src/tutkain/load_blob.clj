(ns tutkain.load-blob
  (:require
   [clojure.repl :as repl]
   [tutkain.format :refer [pp-str Throwable->str]]
   [tutkain.base64 :refer [base64-reader]]
   [tutkain.backchannel :refer [handle respond-to]])
  (:import
   (clojure.lang Compiler)
   (java.io File)))

(defmethod handle :load
  [{:keys [eval-lock eval-context code file] :as message}]
  (try
    (with-open [reader (base64-reader code)]
      (let [file-name (some-> file File. .getName)
            val (locking eval-lock (Compiler/load reader (or file "NO_SOURCE_FILE") file-name))]
        (respond-to message {:tag :ret
                             :val (pp-str val)})))
    (catch Throwable ex
      (swap! eval-context assoc-in [:thread-bindings #'*e] ex)
      (respond-to message {:tag :ret
                           :val (pp-str (assoc (Throwable->map ex) :phase :execution))
                           :exception true}))))

(defmethod handle :transcribe
  [{:keys [eval-lock eval-context code file] :as message}]
  (binding [*ns* (create-ns (gensym "tutkain.transcriptor-"))
            *file* (or file "NO_SOURCE_PATH")
            *source-path* (or (some-> file File. .getName) "NO_SOURCE_FILE")]
    (refer-clojure)
    (respond-to message {:tag :out :val (str (ns-name *ns*) \newline)})
    (with-open [reader (-> code java.io.StringReader. clojure.lang.LineNumberingPushbackReader.)]
      ;; Retain eval lock for the whole transcription process
      (locking eval-lock
        (loop []
          (let [recur? (try
                         (let [[form string] (read+string {:eofthrow false :eof ::eof} reader)]
                           (when-not (identical? ::eof form)
                             (respond-to message {:tag :in :val (str string \newline)})
                             (try
                               (let [ret (eval form)]
                                 (set! *3 *2)
                                 (set! *2 *1)
                                 (set! *1 ret)
                                 (swap! eval-context assoc :thread-bindings (get-thread-bindings))
                                 (respond-to message {:tag :ret :val (pp-str ret)}))
                               true
                               (catch Throwable ex
                                 (set! *e ex)
                                 (swap! eval-context assoc :thread-bindings (get-thread-bindings))
                                 (respond-to message
                                   {:tag :err
                                    :val (Throwable->str ex)
                                    :exception true
                                    :phase :execution})
                                 false))))
                         (catch Throwable ex
                           (set! *e ex)
                           (respond-to message
                             {:tag :err
                              :val (Throwable->str ex)
                              :phase :read-source
                              :exception true})
                           false))]
            (when recur? (recur))))))))

;; test that we're in a tearoff namespace initially
;; test *file* and *source-path*
;; test that clojure is referred
;; test multiple forms
;; test eval lock
;; test thread bindings after successful eval
;; test thread bindings after eval that throws
;; test no recur after throw
;; test *1, *2, *3, *e
;; test read error

(comment
  (handle {:op :transcribe :eval-context (atom {}) :eval-lock (Object.) :code "(inc 1) (inc 2)" :out-fn prn})
  (handle {:op :transcribe :eval-context (atom {}) :eval-lock (Object.) :code "(/ 4 0) (/ 4 4)" :out-fn prn})
  ,,,)
