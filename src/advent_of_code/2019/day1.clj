(ns advent-of-code.2019.day1
  (:require [com.rpl.specter :as s]
            [clojure.string :as str]))
            

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #"\n")
    (s/transform [s/ALL] #(Integer/parseInt %) $)))

(def input (read-input "resources/2019/input1"))

(defn fuel [mass]
  (-> mass (/ 3) (int) (- 2))) 

#_(fuel 1969)

(defn recursive-fuel
  [mass]
  (->> (iterate fuel mass)
       (take-while #(>= % 0))
       (rest)
       (apply +)))

#_(recursive-fuel 1969)

(defn fuel-calculator 
  [massfn]
  (->> (s/transform [s/ALL] massfn input)
       (apply +)))

;;part1
(fuel-calculator fuel)
;;part2
(fuel-calculator recursive-fuel)
