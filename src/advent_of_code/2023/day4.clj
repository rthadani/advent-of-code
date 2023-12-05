(ns advent-of-code.2023.day4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input
  [file]
  (->> (slurp file)
       str/split-lines))

(defn parse-line
  [l]
  (let [[id nos] (str/split l #"\|")
        regex #"\d+"
        first-half (re-seq regex id)
        [card winning-nos] [(first first-half) (rest first-half)]
        my-nos (re-seq regex nos)]
    [card [winning-nos my-nos]]))

(defn winner-counts
  []
  (->> "resources/2023/input4"
     read-input
     (map parse-line)
     (map (fn [[id [w m]]] [id (set/intersection (into #{} w) (into #{} m))])) 
     (map (fn [[i c]] [(Integer/parseInt i) (count c)]))
     (sort-by first)
     (into (sorted-map))))


;;part-1
(->> (winner-counts) 
     (map second)
     (filter pos?)
     (map #(int (Math/pow 2 (dec %))))
     (apply +)) ; 15205

(defn generate-cards
  [winner-counts]
  (->> winner-counts
       (reduce (fn [c [i m]] 
                 (let [cards (filter #(= i (first %)) c)
                       next-cards (for [_ (range (inc (count cards)))
                                        j (range 1 (inc m))]
                                    [(+ i j) (get winner-counts (+ i j) 0)])]
                   (concat (conj c [i m]) next-cards))) [])))

;;part-2
(->> (winner-counts) 
     (generate-cards)
     count) ; 6189740
