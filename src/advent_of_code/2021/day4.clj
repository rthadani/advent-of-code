(ns advent-of-code.2021.day4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn make-card
  [card]
  (let [rows (str/split-lines card)]
    (vec (for [r rows]
           (vec (for [c (str/split r #" ") :when (not-empty c)]
                  (Integer/parseInt (str/trim c))))))))

(defn read-input
  [file]
  (let [[f & r] (-> (slurp file) (str/split #"\n\n"))
        cards (map make-card r)
        numbers (vec (map #(Integer/parseInt (str/trim %)) (str/split f #",")))]
    {:numbers numbers
     :cards  cards}))

(defn win-row-col
  [card called]
  (let [check? #(empty? (set/difference (set %) (set called)))
        row-matched (first  (filter check? card))
        col-matched (first (filter check? (apply mapv vector card)))]
    {:row-match row-matched :col-match col-matched}))

(defn winner?
  [{:keys [row-match col-match]}]
  (or (not-empty row-match) (not-empty col-match)))

(def input (read-input "resources/2021/input4"))

(defn calculate-result
  [[card called]]
  (let [remaining-numbers (set/difference (set (flatten card)) (set called))]
    (* (last called) (apply + remaining-numbers))))

;;4.1
(let [{:keys [numbers cards]} input] ;; => 12796
  (->> (reduce
        (fn  [called n]
          (let [c (conj called n)
                w (first (filter #(winner? (win-row-col % c)) cards))]
            (if w
              (reduced [w c])
              c)))
        []
        numbers)
       calculate-result))

;;4.2
(defn append-board
  [current-wins latest-wins]
  (reduce
   (fn [c l]  (if (some #(= l %) c)
                c
                (conj c l)))
   current-wins
   latest-wins))

(let [{:keys [numbers cards]} input] ;; => 18063
  (->> (reduce
        (fn  [[called winners] n]
          (let [c (conj called n)
                w (filter #(winner? (win-row-col % c)) cards)]
            (if (= (count w) (count cards))
              (reduced [(last (append-board winners w)) c])
              [c (append-board winners w)])))
        [[] []]
        numbers)
       calculate-result))
