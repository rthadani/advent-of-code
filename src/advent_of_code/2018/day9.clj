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
  [{:keys [current-player scores marble scores current-pos placed-marbles]}]
  (let [marble-7-cc (mod (- current-pos 7) (count placed-marbles))
        [f s] [(subvec placed-marbles 0 marble-7-cc) (subvec placed-marbles marble-7-cc)]
        score-7 (first s)]
    {:scores (update scores current-player #(+ % marble score-7))
     :marble (inc marble) 
     :current-pos marble-7-cc
     :placed-marbles (into f (subvec s 1)) 
     :current-player (mod (inc current-player) (count scores))}))

(defn update-marble
  [{:keys [current-player scores marble scores current-pos placed-marbles]}]
  (let [new-marble-pos (mod (+ current-pos 2) (count placed-marbles))]
    {:scores scores
     :marble (inc marble) 
     :current-pos (if (zero? new-marble-pos) (count placed-marbles) new-marble-pos)
     :placed-marbles (if (zero? new-marble-pos) 
                       (conj placed-marbles marble) 
                       (->  (conj (subvec placed-marbles 0 new-marble-pos) marble)
                           (into (subvec placed-marbles new-marble-pos))))
     :current-player (mod (inc current-player) (count scores))}))

(->> (iterate (fn [{:keys [marble] :as game}]
                            (if (zero? (mod marble 23))
                              (update-23-marble game)
                              (update-marble game))) 
                          (game 9))
                 (take 24 )
                 )
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
#_ (part1 411 71170)
#_ (part1 411 7117000)