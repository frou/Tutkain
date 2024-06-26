(ns tutkain.cljs
  (:refer-clojure :exclude [ns-aliases])
  (:require
   [cljs.analyzer :as analyzer]
   [cljs.analyzer.api :as analyzer.api]
   [cljs.repl :as repl]
   [tutkain.completions :as completions]
   [tutkain.lookup :as lookup]
   [tutkain.rpc :refer [respond-to]]
   [tutkain.query :as query]))

(set! *warn-on-reflection* true)

(def ^:dynamic *compiler-env*
  (atom {}))

(defn ^:private format-arglists
  [arglists]
  ;; Why?
  (map pr-str (if (= 'quote (first arglists)) (rest arglists) arglists)))

(defn ^:private shadow-compiler-env
  [build-id]
  (when-some [f (resolve 'shadow.cljs.devtools.api/compiler-env)]
    (atom (f build-id))))

#_(defn ^:private figwheel-compiler-env
  [build-id]
  (some->
    (resolve 'figwheel.main/build-registry)
    deref
    deref
    ;; FIXME: build-id
    (get-in ["dev" :repl-options :compiler-env])))

(defn compiler-env
  ([]
   (or *compiler-env* (analyzer.api/current-state)))
  ([build-id]
   (or (shadow-compiler-env build-id) (compiler-env))))

;; Stolen from Suitable
(def ^:private language-keywords
  #{:require :require-macros :import
    :refer :refer-macros :include-macros
    :refer-clojure :exclude
    :keys :strs :syms
    :as :or
    :pre :post
    :let :when :while

    ;; reader conditionals
    :clj :cljs :default

    ;; common meta keywords
    :private :tag :static
    :doc :author :arglists
    :added :const

    ;; spec keywords
    :req :req-un :opt :opt-un
    :args :ret :fn

    ;; misc
    :keywordize-keys :else :gen-class})

(defn all-keywords
  "Given a compiler environment, return a list of all of the keywords the
  compiler knows about."
  [env]
  (into language-keywords
    (comp
      (map val)
      (map ::analyzer/constants)
      (mapcat :seen)
      (filter keyword?))
    (::analyzer/namespaces @env)))

(comment (all-keywords (compiler-env :node-script)) ,,,)

(defn ns-candidates
  "Given a compiler environment, return all namespace auto-completion
  candidates."
  [env]
  (map completions/annotate-namespace (analyzer.api/all-ns env)))

(defn ^:private ns-aliases
  [env ns]
  (:requires (analyzer.api/find-ns env ns)))

(defn ns-alias-candidates
  "Given a compiler environment and an ns symbol, return all namespace alias
  auto-completion candidates available in the given namespace."
  [env ns]
  (map completions/annotate-namespace (map first (ns-aliases env ns))))

(defn ^:private ns-alias->ns-sym
  [env ns alias]
  (get (ns-aliases env ns) alias))

(defn ^:private var-type
  [{:keys [macro fn-var tag]}]
  (cond
    (= tag 'cljs.core/MultiFn) :multimethod
    macro :macro
    fn-var :function
    :else :var))

(defn ^:private annotate-var
  [{arglists :arglists doc :doc var-name :name :as v}]
  (cond-> {:trigger (name var-name)
           :type (var-type v)}
    (seq arglists) (assoc :arglists (format-arglists arglists))
    (seq doc) (assoc :doc doc)))

(defn scoped-candidates
  "Given a compiler environment, a string prefix, and an ns symbol, return all
  scoped candidates that match the given prefix."
  [env ^String prefix ns]
  (when-some [alias (some-> prefix (.split "/") first symbol)]
    (eduction
      (map val)
      (remove :private)
      (map (fn [v] (update (annotate-var v) :trigger #(str alias "/" %))))
      (some->> (ns-alias->ns-sym env ns alias) (analyzer.api/ns-interns env)))))

(defn ^:private core-candidates
  "Given a compiler environment, return all auto-completion candidates in
  cljs.core."
  [env]
  (eduction
    (map val)
    (remove :private)
    (map annotate-var)
    (analyzer.api/ns-interns env 'cljs.core)))

(defn ^:private ns-var-candidates
  "Given a compiler environment and an ns symbol, return all var
  auto-completion candidates in the namespace."
  [env ns]
  (eduction
    (map val)
    (map annotate-var)
    (analyzer.api/ns-interns env ns)))

(defn ^:private namespace-aliases
  "Given a compiler environment and an ns symbol, return all namespace aliases
  available in ns, excluding any where the alias is the same as the ns name."
  [env ns]
  (into {} (remove #(= (key %) (val %)) (ns-aliases env ns))))

(defn candidates
  "Given a compiler environment, a string prefix, and an ns symbol, return all
  applicable auto-completion candidates for the prefix."
  [env ^String prefix ns]
  (assert (symbol? ns))
  (let [candidates (cond
                     (and (.startsWith prefix "::") (.endsWith prefix "/"))
                     (completions/scoped-auto-resolved-keyword-candidates
                       (all-keywords env)
                       (namespace-aliases env ns)
                       prefix)

                     (and (.startsWith prefix "::") (.contains prefix "/"))
                     (completions/qualified-auto-resolved-keyword-candidates
                       (all-keywords env)
                       (namespace-aliases env ns))

                     (and (.startsWith prefix ":") (.endsWith prefix "/"))
                     (completions/scoped-qualified-keyword-candidates (all-keywords env) prefix)

                     (and (.startsWith prefix ":") (.contains prefix "/"))
                     (completions/qualified-keyword-candidates (all-keywords env))

                     (.startsWith prefix "::")
                     (completions/auto-resolved-keyword-candidates
                       (all-keywords env)
                       (namespace-aliases env ns)
                       ns)

                     (.startsWith prefix ":")
                     (completions/keyword-candidates (all-keywords env))

                     (completions/scoped? prefix)
                     (scoped-candidates env prefix ns)

                     (.contains prefix ".")
                     (ns-candidates env)

                     :else
                     (concat (ns-var-candidates env ns) (core-candidates env) (ns-alias-candidates env ns)))]
    (sort-by :trigger (filter #(completions/candidate? prefix %) candidates))))

(defn ^:private parse-ns
  [ns]
  (or (some-> ns symbol) 'cljs.user))

(defmethod completions/completions :cljs
  [{:keys [ns prefix build-id] :as message}]
  (let [completions (candidates (compiler-env build-id) prefix (parse-ns ns))]
    (respond-to message {:completions completions})))

(comment
  (candidates (compiler-env :browser) "c" 'cljs.core)
  (candidates (compiler-env :browser) "string/" 'cljs.pprint)
  (candidates (compiler-env :browser) "string/b" 'cljs.pprint)
  (candidates (compiler-env :browser) "make-hi" 'cljs.core)
  (candidates (compiler-env :browser) ":a" 'cljs.core)
  ,)

(defn special-sym-meta
  "Given a symbol that names a ClojureScript special form, return metadata for
  that special form."
  [sym]
  (when-some [{:keys [doc forms]} (get repl/special-doc-map sym)]
    {:name (symbol "cljs.core" (str sym))
     :doc doc
     :arglists forms
     :file "cljs/core.cljs"}))

(defn ^:private core-sym-meta
  [env sym]
  (get (analyzer.api/ns-interns env 'cljs.core) sym))

(defn ^:private ns-sym-meta
  [env ns sym]
  (get (analyzer.api/ns-interns env ns) sym))

(defn ^:private qualified-sym-meta
  [env ns ident]
  (when (qualified-symbol? ident)
    (let [ns-alias (-> ident namespace symbol)
          interns (analyzer.api/ns-interns env (ns-alias->ns-sym env ns ns-alias))]
      (-> ident name symbol interns))))

(defn ^:private ns-meta
  [env sym]
  (when (analyzer.api/find-ns env sym)
    (merge (meta sym)
      {:name sym
       :file (some->> sym (analyzer.api/ns-interns env) vals first :file)
       :line 0
       :column 0
       :type :namespace})))

(defn ^:private sym-meta
  [env ns ident]
  (or
    (qualified-sym-meta env ns ident)
    (ns-meta env ident)
    (special-sym-meta ident)
    (core-sym-meta env ident)
    (ns-sym-meta env ns ident)))

(comment
  (def env (compiler-env :browser))
  (sym-meta env 'my.browser.app 'pprint/pprint)
  (sym-meta env 'my.browser.app 'my.browser.app)
  (sym-meta env 'my.browser.app 'let)
  (sym-meta env 'my.browser.app 'mapcat)
  (sym-meta env 'my.browser.app 'start)
  ,,,)

(defn info
  "Given a compiler environment, a symbol that names an ident, and an ns
  symbol, return selected metadata for the named var."
  [env ident ns]
  (let [{:keys [arglists file name] :as ret} (sym-meta env ns ident)]
    (cond-> (select-keys ret [:arglists :doc :file :line :column :name])
      (qualified-symbol? name) (assoc :ns (namespace name))
      arglists (assoc :arglists (format-arglists arglists))
      file (assoc :file (lookup/resolve-file file)))))

(defmethod lookup/info :cljs
  [{:keys [^String ident ns build-id] :as message}]
  (let [env (compiler-env build-id)
        sym (symbol ident)]
    (respond-to message {:info (info env sym (parse-ns ns))})))

(defmethod query/loaded-libs :cljs
  [{:keys [build-id] :as message}]
  (let [env (compiler-env build-id)
        results (eduction
                  (map (partial ns-meta env))
                  (map lookup/prep-meta)
                  (filter :file)
                  (remove (comp #{"NO_SOURCE_PATH"} :file))
                  (analyzer.api/all-ns env))]
    (respond-to message {:results (sort-by (juxt :ns :name) results)})))

(comment
  (->
    (query/loaded-libs {:build-id :browser :out-fn prn :dialect :cljs})
    (with-out-str)
    (read-string))
  ,,,)
