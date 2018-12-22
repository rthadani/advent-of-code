(ns advent-of-code.2018.day11)

(defn power-level 
  [x y serial]
  (-> (+ x 10)
      (* y)
      (+ serial)
      (* (+ x 10))
      (mod 1000)
      (/ 100)
      int
      (- 5)))

(defn make-matrix
  [serial]
  (vec (for [y (range 1 301)]
         (vec (for [x (range 1 301)]
                (power-level x y serial))))))

#_(defn get-box
  [matrix x y block-size]
  (for [y (range y (+ y block-size))
        x (range x (+ x block-size))]
    (get-in matrix [y x])))

(defn get-box
  [matrix x y block-size]
  (for [y (range y (+ y block-size))]
    (subvec (matrix y) x (+ x block-size))))

(defn box-power
  [box]
  (reduce (fn [s row] (+ s (apply + row) )) 0 box))

(defn all-boxes-power
  [block-size matrix]
  (for [y (range 0 (- 300 block-size))
        x (range 0 (- 300 block-size))]
    [[(inc x) (inc y) block-size] (box-power (get-box matrix x y block-size))]))

#_ (def part1 
  (->> (make-matrix 3628)
       (all-boxes-power 3)
       (apply max-key second)
       first
       (take 2)))


#_ (def part2
  (let [matrix (make-matrix 3628)]
    (->> (range 1 300)
         (pmap #(apply max-key second (all-boxes-power % matrix)))
         (apply max-key second)
         first)))

  