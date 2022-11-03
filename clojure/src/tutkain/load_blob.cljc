(ns tutkain.load-blob
  (:require
   [clojure.java.io :as io]
   [tutkain.format :as format :refer [pp-str]]
   [tutkain.base64 :refer [read-base64]]
   [tutkain.backchannel :refer [handle relative-to-classpath-root respond-to]])
  (:import
   (java.io Writer)))

(defmethod handle :load
  [{:keys [eval-lock eval-context code file] :as message}]
  (try
    (let [file-name (some-> file io/file .getName)
          val (locking eval-lock (with-bindings (:thread-bindings @eval-context {})
                                   (let [ret (read-base64 code (relative-to-classpath-root file) file-name)]
                                     (swap! eval-context assoc :thread-bindings (get-thread-bindings))
                                     (.flush ^Writer *err*)
                                     ret)))]
      (respond-to message {:tag :ret
                           :val (pp-str val)}))
    (catch Throwable ex
      (swap! eval-context assoc-in [:thread-bindings #'*e] ex)
      (respond-to message {:tag :err
                           :val (format/Throwable->str ex)
                           :exception true}))))

