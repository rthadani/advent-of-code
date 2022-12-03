(ns advent-of-code.2022.day2
  (:require [clojure.string :as str]))

(defn read-input
 [file]
 (as-> (slurp file) $
    (str/split $ #"\n")
    (mapv #(->> (str/split % #" ") (mapv keyword)) $)))

(def dictionary
  {:A :rock
   :B :paper
   :C :scissors
   :X :rock
   :Y :paper
   :Z :scissors})

(def scores
  {:rock 1
   :paper 2
   :scissors 3})

(def beats 
  {:paper :rock
   :scissors :paper
   :rock    :scissors})

(defn winner 
  [p1 p2]
  (cond 
    (= p1 p2) nil
    (= (beats p1) p2) :p1
    :else :p2))

(defn round-score
  [part1 [other me]]
  (let [other (dictionary other)
        me (-> (if part1 
                  (dictionary me) 
                  (case me
                    :X (beats other)
                    :Y other 
                    :Z (beats(beats other))))) 
                round-goes-to (winner other me) ]
   (cond 
     (nil? round-goes-to) (+ (scores me) 3)
     (= :p1 round-goes-to) (scores me)
     :else (+ 6 (scores me)))))

;part1
(->> (read-input "resources/2022/input2")
     #_[[:A :Y] [:B :X] [:C :Z]]
     (map (partial round-score true))
     (apply +)) ; 14163

;part2 
(->> (read-input "resources/2022/input2")
     #_[[:A :Y] [:B :X] [:C :Z]]
     (map (partial round-score false))
     (apply +)) ; 12091
