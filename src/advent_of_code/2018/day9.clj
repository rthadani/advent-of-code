(ns advent-of-code.2018.day9
  (:require [clojure.string :as str]))


(defn game 
  [players]
  {:scores (zipmap (range 0 players) (repeat players 0))
   :marble 1 
   :current-pos 0
   :placed-marbles [0]
   :current-player 0 
  })

(defn update-23-marble
  [{:keys [current-player scores marble scores current-pos placed-marbles] :as game}]
  (let [marble-7-cc (mod (- current-pos 7) (count placed-marbles))
      [f s] (split-at marble-7-cc placed-marbles)
      score-7 (first s)]
    (println marble-7-cc f s)
    {:scores (update scores current-player #(+ % marble score-7))
     :marble (inc marble) 
     :current-pos marble-7-cc
     :placed-marbles (into [] (concat f (rest s)))
     :current-player (mod (inc current-player) (count scores))}))

(defn update-marble
  [{:keys [current-player scores marble scores current-pos placed-marbles] :as game}]
  (let [new-marble-pos (mod (+ current-pos 2) (count placed-marbles))]
    {:scores scores
     :marble (inc marble) 
     :current-pos (if (zero? new-marble-pos) (count placed-marbles) new-marble-pos)
     :placed-marbles (if (zero? new-marble-pos) 
                       (conj placed-marbles marble) 
                       (into [] (concat (subvec placed-marbles 0 new-marble-pos) [marble] (subvec placed-marbles new-marble-pos))))
     :current-player (mod (inc current-player) (count scores))}))



(defn part1 
  [players max-marble]
  (apply max (-> (iterate (fn [{:keys [marble] :as game}]
                            (if (zero? (mod marble 23))
                              (update-23-marble game)
                              (update-marble game))) 
                          (game players))
                 (nth (inc max-marble))
                 :scores
                 vals)))

#_ (part1 9 25)
#_ (part1 30 5807)