(ns advent-of-code.2021.day5
  (:require [clojure.string :as str]))

(defn point [s]
  (mapv #(Integer/parseInt %) (str/split s #",")))

(defn read-input
  [file]
  (->> (str/split-lines (slurp file))
       (map #(mapv point (str/split % #" -> ")))))

(def input (read-input "resources/2021/input5"))

(defn generate-range [x y]
  (if (< x y)
    (range x (inc y))
    (range x (dec y) -1)))

(defn line
  [[[x1 y1] [x2 y2]] diagonal?]
  (cond
    (= x1 x2) (map vector (repeat x1) (generate-range y1 y2))
    (= y1 y2) (map vector (generate-range x1 x2) (repeat y1))
    diagonal? (map vector (generate-range x1 x2) (generate-range y1 y2))))

;;5.1
(->> (frequencies (mapcat #(line % false) input))  ;; => 6687
     (filter (fn [[_ v]] (> v 1)))
     count)

;;5.2
(->> (frequencies (mapcat #(line % true) input))  ;; => 19851
     (filter (fn [[_ v]] (> v 1)))
     count)
