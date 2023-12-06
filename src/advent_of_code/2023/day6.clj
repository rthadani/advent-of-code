(ns advent-of-code.2023.day6
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (let [[time distance] (->> file slurp str/split-lines)
        time (->> (re-seq #"\d+" time) (map #(Integer/parseInt %)))
        distance (->> (re-seq #"\d+" distance) (map #(Integer/parseInt %)))]
    (mapv vector time distance)))

(defn run-boat
  [hold time]
  (* hold (- time hold)))

(defn wins
  [[time distance]]
  (->> (range time) 
       (filter #(> (run-boat % time) distance))
       count))

(wins [7 9])

;;part 1
(->> (read-input "resources/2023/input6")
     (map wins)
     (apply *)) ;293046

;;this wont work but im too tired
(->> (read-input "resources/2023/input6")
     (reduce (fn [[t d] [it id]] [(str t it) (str d id)]) [])
     (mapv #(Long/parseLong %))
     (map wins))

