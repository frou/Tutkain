(ns tutkain.analyzer
  (:require
   [clojure.tools.analyzer.ast :as analyzer.ast]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as readers]
   [tutkain.rpc :refer [handle]]))

#_(set! *warn-on-reflection* true)

(defn read-forms
  [reader reader-opts start-line start-column]
  (when (and (nat-int? start-line) (nat-int? start-column))
    (into []
      (take-while #(not (identical? ::EOF %)))
      (repeatedly
        #(reader/read (assoc reader-opts :eof ::EOF)
           (readers/->IndexingPushbackReader
             reader start-line start-column true nil 0 *file* false))))))

(defn analyze
  "Read code from a reader and return a reducible of ASTs nodes for the code.

  Keyword arguments:
    :analyzer -- A fn that takes a form and returns an AST
    :forms -- A list of forms to analyze
    :xform -- A transducer to transform resulting sequence of AST nodes (optional)"
  [& {:keys [forms analyzer xform]
      :or {xform (map identity)}}]
  (into []
    (comp
      (take-while #(not (identical? ::EOF %)))
      (map analyzer)
      (mapcat analyzer.ast/nodes)
      xform)
    forms))

(defn ^:private node-form
  [node]
  (or (:form node) (-> node :info :name)))

(defn node->position
  "Given an tools.analyzer node, return the position information for that
  node."
  [node]
  (let [form (node-form node)]
    (some->
      form
      meta
      (select-keys [:line :column :end-column])
      not-empty
      (assoc :form form))))

(defn ^:private node-unique-name
  [node]
  (or (:unique-name node) (:name node)))

(defn local-symbols
  "Given a line number, a column number, and a list of AST nodes, return all
  local symbols in scope at the given line and column."
  [line column nodes]
  (eduction
    (filter (comp #{:fn-method :let :loop} :op))
    (filter (fn [node]
              (let [env (:env node)]
                (<= (:line env) line (:end-line env)))))
    (remove (fn [node]
              (let [env (:env node)]
                (and
                  (= line (:end-line env))
                  (>= column (:end-column env))))))
    (keep (juxt :params :bindings))
    cat
    cat
    (remove (fn [node]
              (let [env (:env node)]
                (and
                  (= line (:line env))
                  (< column (:end-column env))))))
    (keep :form)
    ;; Remove gensym params (e.g. p__11784).
    ;;
    ;;TODO: Is this the right way to do this?
    (remove (fn [form] (re-matches #"\P{N}.*__\p{N}+.*" (name form))))
    (distinct)
    nodes))

(defn index-by-position
  "Given a sequence of tools.analyzer AST nodes, filter nodes that represent a
  local or a binding symbol, and return a map where the key is the position of
  the node and the val is the unique name of the node."
  [nodes]
  (into {}
    (comp
      (filter (every-pred node-form (comp #{:binding :local :letfn} :op)))
      (map (juxt node->position node-unique-name)))
    nodes))

(defn local-positions
  "Given a seq of tools.analyzer AST nodes and map indicating the position of a
  local, return all positions where that local is used."
  [nodes position]
  (let [position->unique-name (index-by-position nodes)]
    (when-some [unique-name (get position->unique-name position)]
      (eduction
        (filter #(= unique-name (node-unique-name %)))
        (map node->position)
        nodes))))

(defn position
  "Given a :locals message, return the position of the local in the message."
  [{:keys [form line column end-column]}]
  {:form (-> form symbol name symbol)
   :line line
   :column column
   :end-column end-column})

(defmulti local-instances :dialect)

(defmethod handle :locals
  [message]
  (local-instances message))
