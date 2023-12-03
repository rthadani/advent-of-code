(ns advent-of-code.2023.day2
  (:require [clojure.string :as str]))


(defn read-input [file]
  (as-> (slurp file) $
        (str/split $ #"\n")))


(defn parse-line
  [game]
  (let [game-id (->> game (re-find #"Game (\d+)") last (Integer/parseInt))
        games (str/split (subs game (+ 2 (.indexOf game ": "))) #";")
        game-regex #"(\d+) (blue|red|green)"]
    [game-id (reduce (fn [a g] (conj a 
                                     (->> g 
                                          (re-seq game-regex) 
                                          (reduce (fn [a g] (assoc a (keyword (last g)) (Integer/parseInt (second g)))) {})
                                          )))
                     [] games)]))

(defn valid?
  [[_ games]]
  (let [max-blocks {:red 12 :green 13 :blue 14}]
    (every? (fn[g] (every? #(<= (% g) (% max-blocks))  (keys g))) games)))


(defn min-valid
  [[_ games]]
  (->> games
       (map #(apply concat %))
       flatten
       (partition-all 2)
       (group-by first)
       (vals)
       (map #(apply max-key second %))
       (map second)))

;;part-1
(->> "resources/2023/input2" 
     (read-input) 
     (map parse-line) 
     (filter valid?)
     (map first)
     (apply +)) ; 1931

;;part-2
(->> "resources/2023/input2" 
     (read-input) 
     (map (comp  min-valid parse-line))
     (map #(apply * %))
     (apply +)) ; 83105

