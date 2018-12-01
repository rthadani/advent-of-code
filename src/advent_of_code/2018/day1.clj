(ns advent-of-code.2018.day1
    (:require [clojure.string :as str]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #"\n")
    (map #(Integer/parseInt %) $)))

(defn result-freq [digits] (apply + digits))

(def input (read-input "resources/2018/input1"))

(def part1 (-> input 
               (result-freq)))

(def part2
  (loop [seen #{0}
         input (cycle input)
         sum 0]
    (let [[current & remaining] input
         new-sum (+ current sum)]
      (if (seen new-sum) 
        new-sum
        (recur (conj seen new-sum)
               remaining
               new-sum)))))