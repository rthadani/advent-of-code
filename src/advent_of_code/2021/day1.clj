(ns advent-of-code.2021.day1
  (:require [clojure.string :as str]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #"\n")
    (map #(Integer/parseInt %) $)))

;;day1.1
(->> (read-input "resources/2021/input1") ;; => 1521
     (partition 2 1)
     (filter (fn [[f s]] (> s f)))
     count)

(let [i (read-input "resources/2021/input1")] ;; => 1521
  (count (filter true? (map < i (rest i)))))

;;day 1.2
(->> (read-input "resources/2021/input1") ;; => 1543
     (partition 3 1)
     (map #(apply + %))
     (partition 2 1)
     (filter (fn [[f s]] (> s f)))
     count)

(let [i (read-input "resources/2021/input1")] ;; => 1543
  (as-> (map + i (next i) (nnext i)) $
    (map < $ (rest $))
    (filter true? $)
    (count $)))
