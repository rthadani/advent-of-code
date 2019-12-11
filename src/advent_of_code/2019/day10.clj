(ns advent-of-code.2019.day10
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/2019/input10")
            (str/split-lines)))

(def asteroids
  (set (for [y (range (count input))
             x (range (count (first input)))
             :when (= (get-in input [y x]) \#)]
         [x y])))

(defn angle
  [[x0 y0] [x1 y1]] (- (Math/atan2 (- x1 x0) (- y1 y0))))

(defn number-of-visible-asteroids [a]
  (->> (disj asteroids a)
       (map (partial angle a))
       (distinct)
       (count)))

;;part 1
(def part1 (->> asteroids
                (map (fn [a] [a (number-of-visible-asteroids a)]))
                (apply max-key second)))

;;part 2
;; cheat here since i know there are atleast 200 points go to the 200th angle
(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y2 y1))))

(let [others (disj asteroids (first part1))
      groups (group-by (fn [a] (angle (first part1) a)) others)
      group (second (nth (sort-by first groups) (dec 200)))]
  (println (nth (sort-by first groups) 200))
  (apply min-key #(manhattan-distance (first part1) %) group))