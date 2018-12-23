(ns advent-of-code.2018.utils)

(defn nth'
  "Like core nth, but when applied to a directly reduceable coll, does not
  force the collection to be realized fully in memory."
  ([coll n]
   (let [result (nth' coll n ::not-found)]
     (if (= result ::not-found)
       (throw (ex-info "Index out of bounds" {:n n}))
       result)))
  ([coll n not-found]
   (if (neg? n)
     not-found
     (transduce (drop n) (completing #(reduced %2)) not-found coll))))