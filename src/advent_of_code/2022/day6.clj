(ns advent-of-code.2022.day6
  (:require [clojure.string :as str]))


(defn read-input 
  [file]
  (slurp file))


(defn char-count-marker-size 
  [size]
  (->> (read-input "resources/2022/input6" )
       #_"bvwbjplbgvbhsrlpgdmjqwftvncz" 
       (partition-all size 1)
       (reduce (fn [a p] 
                 (if (= (count p) (count (distinct p)))
                   (reduced (+ size a))
                   (inc a)))
               0)))

;;part1
(char-count-marker-size 4) ; 1757
;;part2
(char-count-marker-size 14) ; 2950
