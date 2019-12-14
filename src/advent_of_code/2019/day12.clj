(ns advent-of-code.2019.day12)

(def input
  {:x {:positions [14 9 -6 4] :velocities [0 0 0 0]}
   :y {:positions [9 11 14 -4] :velocities [0 0 0 0]}
   :z {:positions [14 6 -4 -3] :velocities [0 0 0 0]}})

(def test-input 
  {:x {:positions [-1 2 4 3] :velocities [0 0 0 0]}
   :y {:positions [0 -10 -8 5] :velocities [0 0 0 0]}
   :z {:positions [2 -7 8 -1] :velocities [0 0 0 0]}})

(defn apply-gravity
  [positions velocities]
  (map 
   (fn [position velocity]
     (->> positions
          (map #(compare % position))
          (apply +)
          (+ velocity)))
   positions velocities))

#_(apply-gravity (get-in test-input [:x :positions]) (get-in test-input [:x :velocities]))

(defn update-positions-velocities
  [{:keys [positions velocities]}]
  (let [velocities (apply-gravity positions velocities)
        positions (map + positions velocities)]
    {:positions positions :velocities velocities}))

(defn energy
  [x y z]
  (->> (interleave x y z)
       (partition 3)
       (map #(map (fn [x] (Math/abs x)) %))
       (map #(apply + %))))

(defn total-energy
  [input steps]
  (let [{xp :positions xv :velocities} (nth (iterate update-positions-velocities (:x input)) steps)
        {yp :positions yv :velocities} (nth (iterate update-positions-velocities (:y input)) steps)
        {zp :positions zv :velocities} (nth (iterate update-positions-velocities (:z input)) steps)
        pe (energy xp yp zp)
        ke (energy xv yv zv)]
    (apply + (map * pe ke))))

;;part1
(total-energy input 1000)

;;part2
(defn period
  [state]
  (inc (.indexOf
        (->> state
             (iterate update-positions-velocities)
             rest)
        state)))


(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))


(reduce lcm (map #(period (get input %)) [:x :y :z]))