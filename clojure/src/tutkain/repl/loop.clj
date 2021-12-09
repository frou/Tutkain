(ns tutkain.repl.loop)

(defn help
  []
  (println "Commands:

    ? — show this help
    q — quit
    . — show current value (without truncation)
    - — go up one level
"))

(defn inspect
  "Given a (presumably nested) coll, start a loop for navigating the coll.

  Reads from *in*, then prints coll at accumulated keypath.

  Input ? for help.

  For all other inputs, print value at (get-in coll (conj keypath key))."
  [coll & {:keys [print print-length print-level]
           :or {print prn print-length 8 print-level 8}}]
  (binding [*print-length* print-length
            *print-level* print-level]
    (help)
    (print coll)
    (loop [keypath []]
      (printf "%s=> " keypath)
      (flush)
      (let [input (read)]
        (println input)
        (cond
          (= input 'q)
          :bye

          (= input '?)
          (do
            (help)
            (recur keypath))

          :else (let [v (get-in coll keypath)]
                  (cond
                    (and (= input 'b) (empty? keypath))
                    (do
                      (print :top)
                      (print v)
                      (recur keypath))

                    (and (= input '-) (empty? keypath))
                    (do
                      (print (get-in coll keypath))
                      (recur keypath))

                    (= input '-)
                    (let [keypath (pop keypath)]
                      (print (get-in coll keypath))
                      (recur keypath))

                    (= input '.)
                    (do
                      (binding [*print-length* nil]
                        (print v))
                      (recur keypath))

                    (or (map? v) (indexed? v))
                    (let [new-keypath (conj keypath input)]
                      (if (contains? (get-in coll keypath) input)
                        (do (print (get-in coll new-keypath))
                          (recur new-keypath))
                        (do (print :nope)
                          (print (get-in coll keypath))
                          (recur keypath))))

                    (and (seqable? v) (nat-int? input) (< input (count v)))
                    (do
                      (print (nth v input))
                      (recur keypath))

                    :else
                    (do
                      (print :nope)
                      (print v)
                      (recur keypath)))))))))

(comment
  (require '[tutkain.repl :refer [*print*]])

  (inspect {:a [{:b (interpose :| (range 32))}]
            :c [{:d 2}]
            :e "foobar"}
    :print *print*)
  ,,,)
