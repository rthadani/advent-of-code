(ns advent-of-code.2024.day1
  (:require [clojure.string :as str]))

(defn read-input
  [input]
  (->> (str/split-lines input)
       (reduce (fn [a l] 
                 (let [[f s] (str/split l #"\s+")]
                   (prn f s)
                   [(conj (first a) (Integer/parseInt (str/trim f))) 
                    (conj (second a) (Integer/parseInt (str/trim s)))])) 
               [[] []])
       ((fn [[f1 f2]] [(sort f1) (sort f2)]))))

(def input (->> "resources/2024/input1" slurp read-input) )

;;part 1
(->> input       
       (apply map #(Math/abs(- %1 %2)))
       (apply +)) ; 1651298

(let [similarity (frequencies (second input))]
  (->> (first input)
       (map #(* (or (get similarity %) 0) %))
       (apply +))) ; 21306195
