(ns custom.utils
  "Namespace for utility functions."
  (:require
   [hlt.utils :refer [log]]))

(def asc
  "Sort in ascending order"
  compare)

(def desc
  "Sort in descending order"
   #(compare %2 %1))

(defn compare-by
  "Returns a comparator to be used when sorting.
  Example: (sort (compare-by :distance asc) coll)"
  [& key-cmp-pairs]
  (fn [x y]
      (loop [[k cmp & more] key-cmp-pairs]
         (let [result (cmp (k x) (k y))]
              (if (and (zero? result) more)
                  (recur more)
                  result)))))

(defmacro defn-timed
  "Creates a function that logs how long it took to execute the body. It supports multiarity functions
  but only times how long the last listed arity version takes. This means it should be used with
  multiarity functions where it calls itself with the extra arguments."
  [fn-name & fn-tail]
  (let [fn-name-str (name fn-name)
        ns-str (str *ns*)
        ;; Extract the doc string from the function if present
        [doc-string fn-tail] (if (string? (first fn-tail))
                               [(first fn-tail) (next fn-tail)]
                               [nil fn-tail])
        ;; Wrap single arity functions in a list
        fn-tail (if (vector? (first fn-tail))
                  (list fn-tail)
                  fn-tail)
        ;; extract other arities defined in the function which will not be timed.
        other-arities (drop-last fn-tail)
        ;; extract the last arity definitions bindings and body
        [timed-arity-bindings & timed-arity-body] (last fn-tail)]
    `(defn ~fn-name
       ~@(when doc-string [doc-string])
       ~@other-arities
       (~timed-arity-bindings
         (let [start# (System/currentTimeMillis)]
           (try
             ~@timed-arity-body
             (finally
               (let [elapsed# (- (System/currentTimeMillis) start#)]
                 (log (format
                        "%s/%s took %d ms." ~ns-str ~fn-name-str elapsed#))))))))))
