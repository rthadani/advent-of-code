(ns advent-of-code.2018.day10
  (:require [clojure.string :as str]))

(defn read-points [s]
  (map (fn [line]
         (zipmap [:x :y :v-x :v-y] (map read-string (re-seq #"-?\d+" line))))
       s))

(def input 
  (->>
    (slurp "resources/2018/input10")
    (str/split-lines)
    read-points))

(defn update-point
    [point]
    (-> point
        (update :x + (:v-x point))
        (update :y + (:v-y point))))

(defn area
  [points]
  (let [min-x (:x (apply min-key :x points))
        min-y (:y (apply min-key :y points))
        max-x (:x (apply max-key :x points))
        max-y (:y (apply max-key :y points))]
    (* (- max-x min-x) (- max-y min-y))))

(defn create-board
  [points]
  (let [min-x (:x (apply min-key :x points))
        min-y (:y (apply min-key :y points))
        max-x (:x (apply max-key :x points))
        max-y (:y (apply max-key :y points))]
    (reduce
     (fn [board {:keys [x y]}] (assoc-in board [(- y min-y) (- x min-x)] "#"))
     (vec (repeat (inc (- max-y min-y)) (vec (repeat (inc (- max-x min-x)) "."))))
     points)))

(defn board->string
  [board]
  (str/join \newline  (map #(str/join "" %) board)))

(defn iterate-board
  [input]
  (->>
   (iterate
    (fn [[points points-area _]]
      (let [new-points (map update-point points)]
        [new-points (area new-points) points-area]))
    [input (area input) (inc (area input))])
   (take-while (fn [[_  a p]]  (< a p)))))
    
(def part1
  (->>  input
        iterate-board 
        last
        first
        create-board
        board->string))

(def part2
  (->>  input
       iterate-board 
       count))
