(ns tutkain.repl.runtime.lookup
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as spec]
   [tutkain.repl.runtime.repl :refer [handle pp-str response-for]]))

(set! *warn-on-reflection* true)

;; Adapted from nrepl.util.lookup

(defn special-sym-meta
  [sym]
  (let [f (requiring-resolve 'clojure.repl/special-doc)]
    (assoc (f sym)
      :ns "clojure.core"
      :file "clojure/core.clj"
      :special-form "true")))

(defn fnspec
  [v]
  (into {}
    (keep #(some->> (get (spec/get-spec v) %) spec/describe pr-str (vector %)))
    [:args :ret :fn]))

(defn sym-meta
  [ns sym]
  (let [var (ns-resolve ns sym)
        fnspec* (fnspec var)]
    (cond-> (if (special-symbol? sym)
              (special-sym-meta sym)
              (some-> var meta))
      (seq fnspec*) (assoc :fnspec fnspec*))))

(defn ^:private remove-empty
  [m]
  (reduce-kv #(if (and (seqable? %3) (empty? %3))
                %1
                (assoc %1 %2 %3))
    {}
    m))

(defn resolve-file
  [file]
  (let [s (str file)]
    (or (some-> s io/resource str) s)))

(defn lookup
  [ns named]
  (if (keyword? named)
    (when-some [spec (some-> named spec/get-spec spec/describe pr-str)]
      {:name named
       :spec spec})
    (let [{sym-name :name sym-ns :ns :as m} (sym-meta ns named)]
      (some->
        m
        (select-keys [:name :file :column :line :arglists :doc :fnspec])
        (assoc :name (symbol (str (ns-name sym-ns)) (name sym-name)))
        (update :file resolve-file)
        (update :arglists #(map pr-str %))
        (remove-empty)))))

(defmulti info :dialect)

(defmethod info :default
  [{:keys [named ns out-fn] :as message}]
  (try
    (let [ns (the-ns (or (some-> ns symbol) 'user))]
      (when-some [result (lookup ns (binding [*ns* ns] (read-string named)))]
        (out-fn (response-for message {:info result}))))
    (catch Throwable ex
      (out-fn (response-for message {:ex (pp-str (Throwable->map ex))})))))

(defmethod handle :lookup
  [message]
  (info message))