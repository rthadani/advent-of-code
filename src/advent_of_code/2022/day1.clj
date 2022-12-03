(ns advent-of-code.2022.day1
  (:require [clojure.string :as str]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #"\n")
    (reduce (fn [r v]  
              (if (empty? v) 
                (conj r [])
                (assoc r 
                       (dec(count r)) 
                       (conj (peek r) (Integer/parseInt v))))) 
            [[]] $)))

(defn calories-per-elf
  []
  (->> (read-input "resources/2022/input1")
  (map #(apply + %)))) 

;part1 
(->> (calories-per-elf) (apply max)) ; 71023

;part2
(->> (calories-per-elf) 
     (sort #(compare %2 %1))
     (take 3)
     (apply +)) ; 206289

