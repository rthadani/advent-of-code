(ns advent-of-code.2023.day7
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (->> (slurp file)
       (str/split-lines)
       (map (fn [l] (let [[c b] (str/split l #" ")]
                      [c (Integer/parseInt b)])))))

(def order-1 [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])
(def order-2 [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J])

(def hand-types {[5] :five-of-a-kind
                 [1 4] :four-of-kind
                 [2 3] :full-house
                 [1 1 3] :three-of-a-kind
                 [1 2 2] :two-pair
                 [1 1 1 2] :one-pair
                 [1 1 1 1 1] :high-card})

(defn hand-type
  [hand consider-joker?]
  (let [hand-to-judge (if (and consider-joker? (str/includes? hand "J") (not-every? #(= % \J) hand))
                        (let [ls (->> hand frequencies (sort-by (comp - second)))
                              max-frequency-letter (str (if (= \J (ffirst ls)) (ffirst (next ls)) (ffirst ls)))]
                         #_(prn (->> hand frequencies (sort-by (comp - second))) max-frequency-letter) 
                          (str/replace hand #"J" max-frequency-letter))
                        hand)]
  (->> hand-to-judge frequencies (sort-by second) (mapv second) hand-types)))

(defn compare-hand-by-high-card
  [h1 h2 order]
  (let [first-different-card-index (->> (map #(vector % %2) h1 h2) 
                                        (take-while #(= (first %) (second %))) count)]
    #_(println h1-order h2-order first-different-card-index)
    (compare (.indexOf order (nth h1 first-different-card-index)) 
             (.indexOf order (nth h2 first-different-card-index)))))

(defn compare-hands
  [order consider-joker? hand1 hand2]
  (let [types (vals hand-types)
        hand1-type (hand-type hand1 consider-joker?)
        hand2-type (hand-type hand2 consider-joker?)
        cht (compare (.indexOf types hand1-type) (.indexOf types hand2-type))]
    #_(println hand1 hand1-type hand2 hand2-type cht)
    (if (zero? cht) (compare-hand-by-high-card hand1 hand2 order) cht)))

(->> (read-input "resources/2023/input7")
     (sort-by first (comp - (partial compare-hands order-1 false)))
     (map-indexed (fn [i [_ b]] (* (inc i) b)))
     (apply +))   ; 250946742

(->> (read-input "resources/2023/input7")
     (sort-by first (comp - (partial compare-hands order-2 true)))
     (map-indexed (fn [i [_ b]] (* (inc i) b)))
     (apply +))  ; 251825891

