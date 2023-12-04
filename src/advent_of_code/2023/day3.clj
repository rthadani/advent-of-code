(ns advent-of-code.2023.day3
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (->> (slurp file)
       str/split-lines
       (mapv vec)))

(defn neighbors
  [v row col]
  (let [cols (count (v 0))
        rows (count v)]
  (for [i (range -1 2)
        j (range -1 2)
        :when (and (not= i j 0 )  
              (>= (+ row i) 0) (< (+ row i) rows)
              (>= (+ col j) 0) (< (+ col j) cols)) ]
    (get-in v [(+ row i) (+ col j)]))))

(defn digit?
  [c]
  (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c))

(defn part?
  [v row col]
  (let [n (neighbors v row col)]
    (boolean  (some #(and (not= \. %) (not (digit? %))) n))))

(defn part-number-positions
  [v]
  (as-> v $
       (reduce (fn [[result row] r]
           (let [positions (into result (for [col (range (count (v 0)))
                                           :when (and (digit? (get-in v [row col])) (part? v row col)) ] 
                                       [row col]))]
             [positions (inc row)])) [#{} 0] $)
       (first $)
       (filter (fn [[r c]] (not (contains? $ [r (dec c)]))) $)
       (sort-by (juxt first second) $)))

(defn generate-part-number
  [v seen-positions [r c]]
  (let [left  (take-while (fn [[col char]] (and (>= col 0) (digit? char))) (iterate (fn [[c char]] [(dec c) (get-in v [r (dec c)])]) [(dec c) (get-in v [r (dec c)])]))
        right (take-while (fn [[col char]] (and (< col (count (v 0))) (digit? char))) (iterate (fn [[c char]] [(inc c) (get-in v [r (inc c)])]) [(inc c) (get-in v [r (inc c)])]))
        positions (into #{} (concat (map (fn [c] [r c]) (map first left)) [[r c]] (map (fn [c] [r c]) (map first right))))
        digits (concat (map second (reverse left)) [(get-in v [r c])] (map second right))]
    (if (seen-positions positions)
      [0 seen-positions]
      [(into seen-positions positions) (str/join "" digits)])))

(defn part-numbers
  [v positions]
  (reduce 
    (fn [[numbers seen-positions] position]
      (let [[seen-positions number] (generate-part-number v seen-positions position)]
      [(conj numbers number) seen-positions]))
    [[] #{}]
    positions))

;;part-1
(let [v (read-input "resources/2023/input3")]
  (->> v 
       part-number-positions
       (part-numbers v)
       first
       (map #(Integer/parseInt %))
       (apply +))) ; 539713
