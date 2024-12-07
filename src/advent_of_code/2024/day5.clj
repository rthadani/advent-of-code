(ns advent-of-code.2024.day5
  (:require [clojure.string :as str]))

(defn read-input 
  [lines]
  (let [[is ls] (str/split lines #"\n\n")]
    [(->> (str/split-lines is)
          (map #(str/split % #"\|"))
          (mapv (fn [[a b]] [(Integer/parseInt a) (Integer/parseInt b)])))
     (->> (str/split-lines ls)
          (mapv #(mapv (fn [i] (Integer/parseInt i)) (str/split % #","))))]))

(def input (read-input (slurp "resources/2024/input5")))

(defn valid?
  [rules ln]
  (every? (fn [i] (some rules #{i})) ln))

;1
(let [[rules lines] input
      in (into #{} rules) ]
  (->> lines
       (filter #(valid? in (partition 2 1 %)))
       (map #(get % (quot (count %) 2)))
       (apply +))) ; 5651

;2
(let [[rules lines] input
      rules (into #{} rules) 
      comparator (fn [a b]
                   (cond 
                     (some rules #{[a b]}) -1
                     (some rules #{[b a]}) 1
                     :else 0))
      invalid-lines (map (comp vec (partial sort comparator)) (remove #(valid? rules (partition 2 1 %)) lines))]
  (->> invalid-lines
       (map #(get % (quot (count %) 2)))
       (apply +))) ; 4743

