(ns advent-of-code.2019.day3
  (:require [com.rpl.specter :as s]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #"\n")
    (s/transform [s/ALL] #(str/split % #",") $)))

#_ (read-input "resources/2019/input3")
#_ (rest (re-find #"(U|D|L|R)(\d+)" "L1007"))
(defn next-points
  [start-point next-line]
  (let [[dir dist] (rest (re-find #"(U|D|L|R)(\d+)" next-line))
        [x y] start-point]
    (for [i (range 1 (inc (Integer/parseInt dist)))]
      (case dir
        "L" [(- x i) y]
        "R" [(+ x i) y]
        "U" [x (- y i)]
        "D" [x (+ y i)]))))
#_ (next-points [0 0] "L5")

(defn wire->points 
  [wire start-point]
  (if (empty? wire)
    []
    (let [points (next-points start-point (first wire))]
      (concat points (wire->points (rest wire) (last points))))))

#_(wire->points ["L5" "U6" "R5" "D6"] [0 0])

(defn wire-intersections
  [wire1 wire2]
  (let [p1 (into #{} (wire->points wire1 [0 0]))
        p2  (into #{} (wire->points wire2 [0 0]))]
    (set/intersection p1 p2)))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn min-dist
  [intersections]
  (let [distances (s/transform [s/ALL] #(manhattan-distance % [0 0]) intersections)]
    (apply min distances)))

(let [[w1 w2] (read-input "resources/2019/input3") ]
  (println "hh" (wire-intersections w1 w2))
  (min-dist (wire-intersections w1 w2)))
