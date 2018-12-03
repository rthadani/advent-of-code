(ns advent-of-code.2018.day2
    (:require [clojure.string :as str]
              [clojure.set :as set]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #"\n")))

(defn letter-frequencies
  [sentence]
  (println sentence)
  (->>
   (frequencies sentence)
   (filter (fn [[k v]] (or (= v 2) (= v 3))))
   (group-by second)
   (map (fn [[k _]] [k 1])) ))

(letter-frequencies "qqwggbohrkplgmcjaxefotvdzns")

(def input (read-input "resources/2018/input2"))

(def part1 (->> input
                (mapcat letter-frequencies)
                (group-by first)
                (vals)
                (map count)
                (apply *)))

(defn different-by
  [box1 box2]
  (->> 
   (map (fn[l1 l2] [l1 l2]) box1 box2)
   (filter (fn [[l1 l2]] (not= l1 l2)))))

(defn different-by-1
  []
  (for [i (range 0 (count input))
        j (range (inc i) (count input))
        :let [ box1 (nth input i)
              box2 (nth input j) 
              diff-by-count (different-by box1 box2)
              _ (if (= 1 (count diff-by-count)) (println box1 box2))]
        :when (= 1 (count diff-by-count))]
     (set/intersection (into #{} box1) (into #{} box1))))

(def part2 (apply str (first (different-by-1))))