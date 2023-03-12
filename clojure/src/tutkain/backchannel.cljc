(ns tutkain.backchannel
  (:require
   [clojure.core.server :as server]
   [clojure.java.io :as io]
   [clojure.main :as main]
   [clojure.edn :as edn]
   [tutkain.format :as format])
  (:import
   (clojure.lang LineNumberingPushbackReader RT)
   (java.nio.file LinkOption Files Paths Path)
   (java.io IOException StringReader Writer)
   (java.net ServerSocket SocketException URL)
   (java.util.concurrent Executors TimeUnit ThreadFactory ThreadPoolExecutor$CallerRunsPolicy)
   (java.util.concurrent.atomic AtomicInteger)))

(comment (set! *warn-on-reflection* true) ,,,)

(defn respond-to
  "Respond to a backchannel op message."
  [{:keys [id out-fn]} response]
  (out-fn (cond-> response id (assoc :id id))))

(defmulti handle
  "Handle a backchannel op message.

  Dispatches on :op."
  :op)

(defmethod handle :echo
  [message]
  (respond-to message {:op :echo}))

(defmethod handle :default
  [message]
  (throw (ex-info "Unknown op" {:message message})))

;; Borrowed from https://github.com/nrepl/nrepl/blob/8223894f6c46a2afd71398517d9b8fe91cdf715d/src/clojure/nrepl/middleware/interruptible_eval.clj#L32-L40
(defn set-column!
  [^LineNumberingPushbackReader reader column]
  #?(:bb nil
     :clj (when-let [^java.lang.reflect.Field field (.getDeclaredField LineNumberingPushbackReader "_columnNumber")]
            (-> field (doto (.setAccessible true)) (.set reader column)))))

(defn ^:private find-or-create-ns
  "Given a namespace symbol, if the namespace named by the symbol already
  exists, return it.

  Otherwise, create a new namespace named by the symbol and refer all public
  vars clojure.core into it."
  [ns-sym]
  (if (nil? ns-sym)
    (the-ns 'user)
    (or (find-ns ns-sym)
    (let [new-ns (create-ns ns-sym)]
      (binding [*ns* new-ns] (refer-clojure))
      new-ns))))

(def ^:private classpath-root-paths
  (delay
    (sequence
      (comp
        (remove #(.contains ^String (.getPath ^URL %) ".jar!"))
        (map #(Paths/get (.toURI ^URL %))))
      (-> (RT/baseLoader) (.getResources "") (enumeration-seq)))))

(defn relative-to-classpath-root
  "Given a string path to a file, return the relative (to a classpath root) path
  to the file.

  If the file doesn't exist, return \"NO_SOURCE_PATH\"."
  [path-str]
  #?(:bb path-str
     :clj
     (if-some [^Path path (some-> path-str not-empty (Paths/get (into-array String [])))]
       (let [^String path-str (if (Files/exists path (into-array LinkOption []))
                                (let [^Path path (.toRealPath path (into-array LinkOption []))]
                                  (if-some [^Path root (some #(when (.startsWith path ^Path %) %) @classpath-root-paths)]
                                    (str (.relativize root path))
                                    path-str))
                                path-str)]
         (.replace path-str "\\" "/"))
       "NO_SOURCE_PATH")))

(defn ^:private make-thread-bindings
  [file ns]
  (let [bindings (cond-> {#'*file* (relative-to-classpath-root file)} ns (assoc #'*ns* (find-or-create-ns ns)))]
    #?(:bb bindings
       :clj (assoc bindings #'*source-path* (or (some-> file io/file .getName) "NO_SOURCE_FILE")))))

(defmethod handle :set-thread-bindings
  [{:keys [thread-bindings ^LineNumberingPushbackReader in ns file line column]
    :or {line 0 column 0} :as message}]
  (.setLineNumber in (int line))
  (set-column! in (int column))
  (swap! thread-bindings merge (make-thread-bindings file ns))
  (respond-to message {:result :ok}))

(defn ^:private make-thread-factory
  [& {:keys [name-suffix]}]
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. runnable (format "tutkain.rpc/%s" (name name-suffix)))
        (.setDaemon true)))))

(defn ^:private make-eval-executor
  []
  (Executors/newSingleThreadExecutor (make-thread-factory :name-suffix :eval)))

(defmethod handle :interrupt
  [{:keys [eval-service ^Thread repl-thread]}]
  (some-> eval-service deref .shutdownNow)
  (when eval-service (reset! eval-service (make-eval-executor)))
  (some-> repl-thread .interrupt))

(defmulti evaluate :dialect)

(defn -update-thread-bindings
  [thread-bindings bindings]
  (swap! thread-bindings merge
    (dissoc bindings #'*file* #?(:bb nil :clj #'*source-path*) #'*ns*)))

(defmethod evaluate :default
  [{:keys [eval-service eval-lock thread-bindings ns file line column code]
    :or {line 1 column 1}
    :as message}]
  (.submit @eval-service
    (bound-fn []
      (locking eval-lock
        (with-bindings (merge
                         {#'*e nil #'*1 nil #'*2 nil #'*3 nil}
                         @thread-bindings
                         (make-thread-bindings file ns))
          (try
            (with-open [reader (-> code StringReader. LineNumberingPushbackReader.)]
              (.setLineNumber reader (int line))
              (set-column! reader (int column))
              (run!
                (fn [form]
                  (try
                    (let [ret (eval form)]
                      (.flush ^Writer *out*)
                      (.flush ^Writer *err*)
                      (set! *3 *2)
                      (set! *2 *1)
                      (set! *1 ret)
                      (-update-thread-bindings thread-bindings (get-thread-bindings))
                      (respond-to message {:tag :ret :val (format/pp-str ret)}))
                    (catch Throwable ex
                      (.flush ^Writer *out*)
                      (.flush ^Writer *err*)
                      (set! *e ex)
                      (-update-thread-bindings thread-bindings (get-thread-bindings))
                      (respond-to message {:tag :err :val (format/Throwable->str ex)}))))
                (take-while #(not= % ::EOF)
                  (repeatedly #(read {:read-cond :allow :eof ::EOF} reader)))))
            (catch Throwable ex
              (set! *e ex)
              (-update-thread-bindings thread-bindings (get-thread-bindings))
              (respond-to message {:tag :err :val (format/Throwable->str ex)}))))))))

(defmethod handle :eval
  [message]
  (evaluate message))

(defonce ^:private ^AtomicInteger thread-counter
  (AtomicInteger.))

(defn ^:private make-debouncer
  [service]
  (fn [f delay]
    (let [task (atom nil)]
      (fn [& args]
        (some-> @task (.cancel false))
        (reset! task
          (.schedule service
            (fn []
              (apply f args)
              (reset! task nil))
            delay
            TimeUnit/MILLISECONDS))))))

(defn accept
  [{:keys [add-tap? eventual-out-writer eventual-err-writer thread-bindings xform-in xform-out greet?]
    :or {add-tap? false xform-in identity xform-out identity greet? true}}]
  (let [out *out*
        lock (Object.)
        out-fn (fn [message]
                 (binding [*print-length* nil
                           *print-level* nil
                           *print-meta* false
                           *print-namespace-maps* false
                           *print-readably* true]
                   (locking lock
                     (.write out (pr-str (dissoc (xform-out message) :out-fn :thread-bindings)))
                     (.write out "\n")
                     (.flush out))))
        tapfn #(out-fn {:tag :tap :val (format/pp-str %1)})
        debounce-service (doto (Executors/newScheduledThreadPool 1 (make-thread-factory :name-suffix :debounce))
                           (.setRejectedExecutionHandler (ThreadPoolExecutor$CallerRunsPolicy.)))
        eval-service (atom (make-eval-executor)) ; ClojureScript does not use this. Add option to disable?
        debounce (make-debouncer debounce-service)]
    (when add-tap? (add-tap tapfn))
    (let [out-writer (PrintWriter-on #(out-fn {:tag :out :val %1}) nil)
          err-writer (PrintWriter-on #(out-fn {:tag :err :val %1}) nil)
          flush-out (debounce #(.flush out-writer) 50)
          flush-err (debounce #(.flush err-writer) 50)
          write-out (fn [string] (.write out-writer string) (flush-out))
          write-err (fn [string] (.write err-writer string) (flush-err))]
      (deliver eventual-out-writer write-out)
      (deliver eventual-err-writer write-err)
      (with-bindings @thread-bindings
        (try
          (binding [*out* (PrintWriter-on write-out #(.close out-writer))
                    *err* (PrintWriter-on write-err #(.close err-writer))]
            (when greet? (out-fn {:tag :out :val (str "Clojure " (clojure-version) "\n")}))
            (loop []
              (let [recur?
                    (try
                      (let [message (edn/read {:eof ::EOF} *in*)]
                        (if (or (identical? ::EOF message) (= :quit (:op message)))
                          false
                          (let [message (assoc (xform-in message)
                                          :eval-service eval-service
                                          :thread-bindings thread-bindings
                                          :out-fn out-fn)]
                            (try
                              (handle message)
                              (.flush ^Writer *err*)
                              true
                              (catch Throwable ex
                                (respond-to message {:tag :ret
                                                     :exception true
                                                     :val (format/pp-str (Throwable->map ex))})
                                (.flush ^Writer *err*)
                                true)))))
                      ;; If we can't read from the socket, exit the loop.
                      (catch #?(:bb clojure.lang.ExceptionInfo :clj clojure.lang.EdnReader$ReaderException) _ false)
                      (catch SocketException _ false)
                      ;; If the remote host closes the connection, exit the loop.
                      (catch IOException _ false))]
                (when recur? (recur)))))
          (finally
            (some-> debounce-service .shutdownNow)
            (some-> eval-service deref .shutdownNow)
            (remove-tap tapfn)))))))

(defprotocol Backchannel
  (thread-bindings [this])
  (update-thread-bindings [this thread-bindings])
  (host [this])
  (port [this])
  (write-out [this x])
  (write-err [this x])
  (close [this]))

(defn ^:private init-thread-bindings
  [bindings]
  (atom (select-keys bindings [#'*e #'*1 #'*2 #'*3 #'*warn-on-reflection*])))

(defn open
  "Open a backchannel that listens for editor tooling messages on a socket.

  Editor tooling messages are EDN messages that look like nREPL ops. For
  example:

    {:op :load-base64 :blob \"...\" :file \"foo.clj\"}

  To add a new op, implement the tutkain.backchannel/handle multimethod.

  Options:
    :port         The TCP port the backchannel listens on.
    :bind-address The TCP bind address.

  Other options are subject to change.

  Returns a Backchannel instance."
  [{:keys [add-tap? bind-address port bindings xform-in xform-out]
      :or {add-tap? false bind-address "localhost" port 0 xform-in identity xform-out identity}}]
  (let [thread-bindings (init-thread-bindings bindings)
        out-writer (promise)
        err-writer (promise)
        server-name (format "tutkain/backchannel-%s" (.incrementAndGet thread-counter))
        ^ServerSocket socket (server/start-server
                               {:address bind-address
                                :port port
                                :name server-name
                                :accept `accept
                                :args [{:add-tap? add-tap?
                                        :thread-bindings thread-bindings
                                        :eventual-out-writer out-writer
                                        :eventual-err-writer err-writer
                                        :xform-in #(xform-in %)
                                        :xform-out #(xform-out %)}]})]
    (reify Backchannel
      (thread-bindings [_] @thread-bindings)
      (update-thread-bindings [_ bindings]
        (-update-thread-bindings thread-bindings bindings))
      (host [_] (-> socket .getInetAddress .getHostName))
      (port [_] (.getLocalPort socket))
      (write-out [_ x] (@out-writer x))
      (write-err [_ x] (@err-writer x))
      (close [_] (server/stop-server server-name)))))

(defn default-init
  []
  (in-ns 'user)
  (apply require main/repl-requires))

(defn rpc
  [{:keys [init] :or {init `default-init} :as opts}]
  (let [eval-lock (Object.)]
    (when-some [initf (or
                        (try (requiring-resolve init) (catch java.io.FileNotFoundException _))
                        (requiring-resolve `default-init))]
      (initf))
    (accept
      (assoc opts
        :xform-in #(assoc % :eval-lock eval-lock)
        :thread-bindings (init-thread-bindings {})
        :eventual-out-writer (promise)
        :eventual-err-writer (promise)))))
