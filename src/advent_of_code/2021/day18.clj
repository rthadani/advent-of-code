(ns advent-of-code.2021.day18
  (:require
   [clojure.string :as str]
   [clojure.zip :as z]))

(defn read-input
  [file]
  (->> file slurp str/split-lines (map read-string)))

(def input (read-input "resources/2021/input18"))

(defn root
  [zipper]
  (z/vector-zip (z/root zipper)))

(defn find-node
  [pred zipper]
  (loop [z zipper]
    (cond
      (z/end? z) nil
      (pred z) z
      :else (recur (z/next z)))))

(defn right-number
  [zipper]
  (find-node (comp int? z/node) (z/next zipper)))

(defn left-number
  [zipper]
  (when-let [z1 (z/prev zipper)]
    (if (int? (z/node z1)) z1 (recur z1))))

(defn explode
  [zipper]
  (if-let [explode?
           (find-node (fn [z] (and (= 4 (count (z/path z)))
                                   (vector? (z/node z))))
                      zipper)]
    (let [[l r] (z/node explode?)
          z1 (z/replace explode? 0)
          _ (println z1)
          z2 (if-let [zl (left-number z1)]
               (right-number (z/edit zl + l))
               z1)]
      (if-let [zr (right-number z2)]
        (left-number (z/edit zr + r))
        z2))
    (root zipper)))

#_(z/root (explode (z/vector-zip [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])))
#_(z/root (explode (z/vector-zip [[[[0,7],4],[7,[[8,4],9]]],[1,1]]))) ;; => [[[[0 7] 4] [15 [0 13]]] [1 1]]

(defn split
  [zipper]
  (if-let [split?
           (find-node (fn [z] (and (int? (z/node z))
                                   (> (z/node z) 9)))
                      zipper)]
    (z/edit split?
            (fn [x] [(int (/ x 2)) (int (Math/ceil (/ x 2)))]))
    (root zipper)))

(defn evaluate-expression
  [zipper]
  (let [exp (explode zipper)]
    (if (not= exp zipper)
      (recur exp)
      (let [spl (split zipper)]
        (if (not= spl zipper)
          (recur spl)
          (z/root zipper))))))

(defn magnitude [x]
  (if (vector? x)
    (+ (* 3 (magnitude (first  x))) (* 2 (magnitude (second x))))
    x))

(def snail-math (comp evaluate-expression z/vector-zip vector))

;;18.1
(->> input ;; => 3725
     (reduce snail-math)
     magnitude)

;;18.2
(->> (for [x input ;; => 4832
           y input]
       (magnitude (snail-math x y)))
     (apply max))
