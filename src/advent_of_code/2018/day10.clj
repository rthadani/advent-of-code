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
  [{:keys [x y v-x v-y]}]
  {:x (+ x v-x) :y (+ y v-y) :v-x v-x :v-y v-y})

(defn area
  [points]
  (let [min-x (:x (apply min-key :x points))
        min-y (:y (apply min-key :y points))
        max-x (:x (apply max-key :x points))
        max-y (:y (apply max-key :y points))]
    (Math/abs (* (- min-x max-x) (- min-y max-y)))))


(defn create-board
  [points]
  (let [min-x (:x (apply min-key :x points))
        min-y (:y (apply min-key :y points))
        max-x (:x (apply max-key :x points))
        max-y (:y (apply max-key :y points))] 
       (println (- max-x min-x) (- max-y min-y))
       (reduce
        (fn [board {:keys [x y]}] (assoc-in board [(- y min-y) (- x min-x)] "#"))
        (vec (repeat (inc (- max-y min-y)) (vec (repeat (inc (- max-x min-x)) "."))))
        points)))

(defn board->string
  [board]
  (str/join \newline (map #(str/join "" %) board)))


(defn part1
  [input]
  (->> (iterate
        (fn [[points current-area]]
          (let [new-points (map update-point points)]
            [new-points (area new-points) current-area]))
        [input (area input) (inc (area input))])
       (take-while (fn [[_  a p]]  (< a p)))
       last
       first
       create-board
       board->string))

(def test-input
  (read-points 
   [
"position=< 9,  1> velocity=< 0,  2>"
"position=< 7,  0> velocity=<-1,  0>"
"position=< 3, -2> velocity=<-1,  1>"
"position=< 6, 10> velocity=<-2, -1>"
"position=< 2, -4> velocity=< 2,  2>"
"position=<-6, 10> velocity=< 2, -2>"
"position=< 1,  8> velocity=< 1, -1>"
"position=< 1,  7> velocity=< 1,  0>"
"position=<-3, 11> velocity=< 1, -2>"
"position=< 7,  6> velocity=<-1, -1>"
"position=<-2,  3> velocity=< 1,  0>"
"position=<-4,  3> velocity=< 2,  0>"
"position=<10, -3> velocity=<-1,  1>"
"position=< 5, 11> velocity=< 1, -2>"
"position=< 4,  7> velocity=< 0, -1>"
"position=< 8, -2> velocity=< 0,  1>"
"position=<15,  0> velocity=<-2,  0>"
"position=< 1,  6> velocity=< 1,  0>"
"position=< 8,  9> velocity=< 0, -1>"
"position=< 3,  3> velocity=<-1,  1>"
"position=< 0,  5> velocity=< 0, -1>"
"position=<-2,  2> velocity=< 2,  0>"
"position=< 5, -2> velocity=< 1,  2>"
"position=< 1,  4> velocity=< 2,  1>"
"position=<-2,  7> velocity=< 2, -2>"
"position=< 3,  6> velocity=<-1, -1>"
"position=< 5,  0> velocity=< 1,  0>"
"position=<-6,  0> velocity=< 2,  0>"
"position=< 5,  9> velocity=< 1, -2>"
"position=<14,  7> velocity=<-2,  0>"
"position=<-3,  6> velocity=< 2, -1>" ]))

 (println 
    (part1 input))
