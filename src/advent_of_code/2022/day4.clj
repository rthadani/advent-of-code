(ns advent-of-code.2022.day4
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(re-find(re-matcher #"(\d+)-(\d+),(\d+)-(\d+)" %)))
       (map #(vec (rest  %)))
       (map #(mapv (fn [r] (Integer/parseInt r)) %))
       (map (fn [r] [(subvec r 0 2) (subvec r 2 4)]))))


(defn overlap
  [[[s1 e1] [s2 e2]]]
  (or (and (<= s1 s2) (<= e2 e1))
      (and (<= s2 s1) (<= e1 e2) )))

(defn overlap2
  [[[s1 e1] [s2 e2]]]
  (or (and (<= s1 s2) (<= s2 e1))
      (and (<= s2 s1) (<= s1 e2))))

;part1
(->> (read-input "resources/2022/input4")
     (filter overlap)
     count) ; 471

(->> (read-input "resources/2022/input4")
     (filter overlap2)
     count) ; 888
