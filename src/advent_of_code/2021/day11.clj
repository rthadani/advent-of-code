(ns advent-of-code.2021.day11
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (vec (for [i (-> file slurp str/split-lines)]
         (vec (for [j i]
                (Integer/parseInt (str j)))))))

(def input (read-input "resources/2021/input11"))

(defn increase-energy
  [matrix]
  (vec (for [i (range (count matrix))]
         (vec (for [j (range (count (matrix 0)))]
                (inc (get-in matrix [i j])))))))

(defn neighbor-offsets
  [mat x y]
  (for [ox (range -1 2)
        oy (range -1 2)
        :when (and (not= [0 0] [ox oy])
                   (>= (+ x ox) 0)
                   (>= (+ y oy) 0)
                   (< (+ x ox) (count mat))
                   (< (+ y oy) (count (first mat))))]
    [(+ x ox) (+ y oy)]))

(defn flash-on-large-energy
  [[m flashes] [i j]]
  (if (> (get-in m [i j]) 9)
    [(-> (reduce (fn [m [x y]]
                   (if (pos? (get-in m [x y]))
                     (update-in m [x y] inc)
                     m)) m (neighbor-offsets m i j))
         (assoc-in [i j] 0))
     (inc flashes)]
    [m flashes]))

(defn step
  [mat flashes]
  (loop [[m f] [(increase-energy mat) flashes]
         p -1]
    (if (= p f)
      [m f]
      (recur  (reduce flash-on-large-energy [m f]
                      (for [i (range 0 (count mat)) j (range 0 (count (first mat)))] [i j]))
              f))))

;;11.1
(->> (iterate #(apply step %) [input 0]) ;; => 1681
     (take 101)
     last
     second)
;;11.2
(->> (iterate #(apply step %) [input 0]) ;; => 276
     (take-while #(not (every? zero? (flatten (first %)))))
     count)
