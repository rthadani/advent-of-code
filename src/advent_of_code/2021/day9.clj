(ns advent-of-code.2021.day9
  (:require
   [clojure.string :as str]
   [clojure.data.priority-map :refer [priority-map]]))

(defn read-input
  [file]
  (vec (for [i (-> file slurp str/split-lines)]
         (vec (for [j i]
                (Integer/parseInt (str j)))))))

(def input (read-input "resources/2021/input9"))

(defn neighbors
  [mat x y]
  (for [[ox oy] [[0 -1] [0 1] [-1 0] [1 0]]
        :when (and (>= (+ x ox) 0) (>= (+ y oy) 0) (< (+ x ox) (count mat)) (< (+ y oy) (count (first mat))))]
    (get-in mat [(+ x ox) (+ y oy)])))

(defn low-points
  [mat]
  (reduce
   (fn [low-points [x y]]
     (let [neighbors (neighbors mat x y)]
       (if (every? #(< (get-in mat [x y]) %) neighbors)
         (conj low-points [x y])
         low-points)))
   []
   (for [i (range 0 (count mat)) j (range 0 (count (first mat)))] [i j])))

;;9.1
(reduce ;; => 486
 (fn [sum xy]
   (+ sum (get-in input xy) 1))
 0
 (low-points input))

;;9.2
(defn basin
  ([x y]
   (basin x y #{}))
  ([x y visited]
   (if (or
        (visited [x y])
        (< x 0)
        (< y 0)
        (>= x (count input))
        (>= y (count (first input)))
        (= 9 (get-in input [x y])))
     visited
     (->>
      (conj visited [x y])
      (basin (dec x) y)
      (basin (inc x) y)
      (basin x (dec y))
      (basin x (inc y))))))

(->>
 (low-points input)
 (reduce
  (fn [basins [x y]] (assoc basins [x y] (count  (basin x y))))
  (priority-map))
 (rseq)
 (map second)
 (take 3)
 (apply *))
