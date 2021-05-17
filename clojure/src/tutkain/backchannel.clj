(ns tutkain.backchannel
  (:import
   (clojure.lang Compiler Compiler$CompilerException LineNumberingPushbackReader)
   (java.io ByteArrayInputStream InputStreamReader)
   (java.lang.reflect Field)
   (java.net InetSocketAddress)
   (java.nio.channels Channels ServerSocketChannel)
   (java.util Base64)))

(defn respond-to
  [{:keys [id out-fn]} response]
  (out-fn (cond-> response id (assoc :id id))))

(defmulti handle :op)

(defmethod handle :echo
  [message]
  (respond-to message {:op :echo}))

(defmethod handle :default
  [message]
  (throw (ex-info "Unknown op" {:message message})))

(def ^:private base64-decoder
  (Base64/getDecoder))

(defn ^:private base64-reader
  [blob]
  (->
    base64-decoder
    (.decode blob)
    (ByteArrayInputStream.)
    (InputStreamReader.)
    (LineNumberingPushbackReader.)))

(defmethod handle :load-base64
  [{:keys [blob path filename] :as message}]
  (with-open [reader (base64-reader blob)]
    (try
      (Compiler/load reader path filename)
      (respond-to message {:filename filename :result :ok})
      (catch Compiler$CompilerException ex
        (respond-to message {:filename filename :result :fail :reason :compiler-ex :ex (Throwable->map ex)})))))

(def ^:private eval-ctx
  (atom {:file nil}))

;; Borrowed from https://github.com/nrepl/nrepl/blob/8223894f6c46a2afd71398517d9b8fe91cdf715d/src/clojure/nrepl/middleware/interruptible_eval.clj#L32-L40
(defn- set-column!
  [^LineNumberingPushbackReader reader column]
  (when-let [field (->> LineNumberingPushbackReader
                     (.getDeclaredFields)
                     (filter #(= "_columnNumber" (.getName ^Field %)))
                     first)]
    (-> ^Field field
      (doto (.setAccessible true))
      (.set reader column))))

(defmethod handle :set-eval-context
  [{:keys [in file line column] :or {line 0 column 0} :as message}]
  (.setLineNumber in (int line))
  (set-column! in (int column))
  (let [new-context (swap! eval-ctx assoc :file file)]
    (respond-to message new-context)))

(defn eval-context
  [k]
  (get @eval-ctx k))

(defn open
  [thread-name {:keys [port xform-in xform-out] :or {port 0 xform-in identity xform-out identity}}]
  (let [socket (ServerSocketChannel/open)
        address (InetSocketAddress. "localhost" port)
        repl-thread (Thread/currentThread)
        lock (Object.)]
    (.bind socket address)
    (let [thread (Thread.
                   (fn []
                     (try
                       (let [socket-channel (.accept socket)
                             in (LineNumberingPushbackReader. (Channels/newReader socket-channel "UTF-8"))
                             out (Channels/newWriter socket-channel "UTF-8")
                             EOF (Object.)
                             out-fn (fn [message]
                                      (locking lock
                                        (.write out (pr-str (dissoc (xform-out message) :out-fn)))
                                        (.write out "\n")
                                        (.flush out)))]
                         (loop []
                           (when (.isOpen socket)
                             (let [message (read in false EOF)]
                               (when-not (identical? EOF message)
                                 (case (:op message)
                                   :interrupt (.interrupt repl-thread)
                                   (handle (assoc (xform-in message) :out-fn out-fn)))
                                 (recur))))))
                       (finally
                         (.close socket)))))]
      (doto thread
        (.setName thread-name)
        (.setDaemon true)
        (.start))
      socket)))
