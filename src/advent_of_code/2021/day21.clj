(ns advent-of-code.2021.day21)

(def input {:p1 {:pos 7 :score 0}
            :p2 {:pos 4 :score 0}
            :die 0
            :rolls 0
            :turn :p1})

(defn player-turn
  [{:keys [p1 p2 die rolls turn] :as state}]
  (let [next-rolls (map #(mod (+ die %) 100) (range 1 4))
        current-position (get-in state [turn :pos])
        next-position (mod (apply + (conj next-rolls current-position)) 10)
        next-position (if (zero? next-position) 10 next-position)
        score (+ (get-in state [turn :score]) next-position)]
    (-> {:p1 p1
         :p2 p2
         :die (last next-rolls)
         :rolls (+ rolls 3)
         :turn (if (= :p1 turn) :p2 :p1)}
        (assoc-in [turn :pos] next-position)
        (assoc-in [turn :score] score))))

(defn answer
  [{:keys [p1 p2 rolls]}]
  (if (< (:score p1) (:score p2))
    (* (:score p1) rolls)
    (* (:score p2) rolls)))

;21.1
(->> input ;; => 675024
     (iterate player-turn)
     (drop-while (fn [{:keys [p1 p2]}] (and (< (:score p1) 1000) (< (:score p2) 1000))))
     first
     answer)

;21.2
(def quantum-game
  (memoize
   (fn [p1 p2 score-1 score-2]
     (if (>= score-2 21)
       [0 1]
       (reduce #(mapv + %1 %2)
               (for [u1 [1 2 3]
                     u2 [1 2 3]
                     u3 [1 2 3]
                     :let [roll   (+ u1 u2 u3)
                           next-pos (inc (mod (+ p1 roll -1) 10))
                           score (+ score-1 next-pos)]]
                 (reverse (quantum-game p2 next-pos score-2 score))))))))

(apply max (quantum-game 7 4 0 0)) ;; => 570239341223618
