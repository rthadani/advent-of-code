(ns advent-of-code.2018.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def regex #"(\d+), (\d+)")
(def input
  (->>
   (slurp "resources/2018/input6")
   (str/split-lines)
   (map #(re-find regex %))
   (map (fn [[_ x y]] [(Integer/parseInt x) (Integer/parseInt y)]))))

(defn all-except-input-coords
  [input]
  (let [min-x (first (apply min-key first input))
      min-y (second (apply min-key second input))
      max-x (first (apply max-key first input))
      max-y (second (apply max-key second input))]
    [min-x max-x min-y max-y 
     (set/difference
      (into #{} (for [i (range min-x (inc max-x))
                      j (range min-y (inc max-y))]
                  [i j]))
      (into #{} input))]))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y2 y1))))

(defn nearest-input-for-xy
  [[x y] input]
  (let [distance-calc (partial manhattan-distance [x y])
        min-distance-point (apply min-key distance-calc input)
        other-min-distance-point (apply min-key distance-calc (remove #(= min-distance-point %) input))]
    (if (= (distance-calc other-min-distance-point) (distance-calc min-distance-point))
      [[0 0] [x y]]
      [min-distance-point [x y]])))

(defn discard-point?
  [point min-x min-y max-x max-y]
  (or (= [0 0] point)
      (<= (first point) min-x)
      (>= (first point) max-x)
      (<= (second point) min-y)
      (>= (second point) max-y)))

(def part1
  (let [[min-x max-x min-y max-y i] (all-except-input-coords input)
        nearest-map (map #(nearest-input-for-xy % input) i)]
    (->> nearest-map
      (filter #(not (discard-point? (first %) min-x min-y max-x max-y)))
         (group-by first)
         (map (fn [[_ vs]] (count vs)))
         (apply max)
         (inc))))