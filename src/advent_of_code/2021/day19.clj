(ns advent-of-code.2021.day19
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (->> (slurp file)
       ((fn [lines] (str/split lines #"\n\n")))
       (map str/split-lines)
       (map (fn [[_ & coords]]
              (map #(read-string (str "[" % "]")) coords)))))

(defn transpose [mat]
  (apply mapv vector mat))

(defn permute
  [xs]
  (letfn [(rotations [[x y z]]
            [[x    y     z]
             [x (- z)    y]
             [x (- y) (- z)]
             [x    z  (- y)]])
          (faces [[x y z]]
            [[x        y     z]
             [y     (- x)    z]
             [(- x) (- y)    z]
             [(- y)    x     z]
             [y        z     x]
             [y     (- z) (- x)]])]
    (transpose
     (map (fn [pt] (apply concat
                          (for [r (faces pt)]
                            (rotations r))))
          xs))))

(defn sub
  [[x y z] [x' y' z']]
  [(- x x') (- y y') (- z z')])

(defn add
  [[x y z] [x' y' z']]
  [(+ x x') (+ y y') (+ z z')])

(defn overlap
  [xs ys]
  (->> (for [x xs, y ys] (sub x y))
       (map (fn [v] {v 1}))
       (apply merge-with +)
       (filter (fn [[_ v]] (>= v 12)))
       (map first)))

(defn match
  [xs ys]
  (for [perm-ys (permute ys)
        pos     (overlap xs perm-ys)]
    {:scanner pos
     :points  (map (partial add pos) perm-ys)}))

(defn align
  [res [ref & refs] scanners]
  (if (empty? scanners)
    res
    (let [{found true, not-found false}
          (group-by (comp some? :found)
                    (for [scanner scanners
                          :let [ali (match ref scanner)]]
                      (if (empty? ali)
                        {:not-found scanner}
                        {:found (first ali)})))]
      (recur (concat (map :found found) res)
             (concat (map :points (map :found found)) refs)
             (map :not-found not-found)))))

(defn manhattan [[x y z] [x* y* z*]]
  (+ (+ (Math/abs (- x x*))
        (Math/abs (- y y*)))
     (Math/abs (- z z*))))

(def input (read-input "resources/2021/input19"))

(def scanners
  (let [initial (first input)
        beacons (rest  input)]
    (align [{:scanner [0 0 0], :points initial}]
           [initial]
           beacons)))

#_(->> scanners ; => 306
       (map :points)
       (apply concat)
       (into #{})
       count)

#_(apply max (for [p (map :scanner scanners) q (map :scanner scanners)] ; => 9764 
               (manhattan p q)))
