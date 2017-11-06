(ns custom.utils
  "Namespace for utility functions.")

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
