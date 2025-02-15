(ns advent-of-code.2024.day8
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

(defn read-input [lines]
  (->> (str/split-lines lines)
       (mapv vec)))

(def input (read-input (slurp "resources/2024/input8")))

(defn station-locations
  [input]
  (let [width (count input)
        locations (set (for [y (range width) x (range width)] [y x]))]
    [(-> (reduce #(update %1 (get-in input %2) conj %2) {} locations)
        (dissoc \.))
     locations]))

(station-locations input)

(defn dydx* [[[y x] [y' x']]] [(- y' y) (- x' x)])

(defn antinodes 
  [station-pair locations]
  (let [dydx (dydx* station-pair)]
    (->> (concat (mapv #(mapv - % dydx) station-pair) (mapv #(mapv + % dydx) station-pair))
         (remove (set station-pair))
         (filter locations))))

;1
(let [[station-locs locations] (station-locations input)] 
  (->> (vals station-locs)
     (map (fn [locs] (combo/combinations locs 2)))
     (map (fn [combos] (mapcat (fn [c] (antinodes c locations)) combos)))
     (reduce into #{})
     count)) ; 259

(defn antinodes-freq
  [station-pair locations] 
  (let [dydx (dydx* station-pair)]
    (into
     (take-while locations (iterate #(mapv - % dydx) (first station-pair)))
     (take-while locations (iterate #(mapv + % dydx) (first station-pair))))))

;2
(let [[station-locs locations] (station-locations input)] 
  (->> (vals station-locs)
       (map #(combo/combinations % 2))
       (map #(mapcat antinodes-freq % locations))
       (reduce into #{})
       count))
