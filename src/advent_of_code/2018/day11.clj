(ns advent-of-code.2018.day11)

; For example, to find the power level of the fuel cell at 3,5 in a grid with serial number 8:

;     The rack ID is 3 + 10 = 13.
;     The power level starts at 13 * 5 = 65.
;     Adding the serial number produces 65 + 8 = 73.
;     Multiplying by the rack ID produces 73 * 13 = 949.
;     The hundreds digit of 949 is 9.
;     Subtracting 5 produces 9 - 5 = 4.

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

(defn get-box
  [matrix x y block-size]
  (for [y (range y (+ y block-size))
        x (range x (+ x block-size))]
    (get-in matrix [y x])))

(defn box-power
  [box]
  (apply + box))

(defn all-boxes-power
  [block-size matrix]
  (for [y (range 0 (- 300 block-size))
        x (range 0 (- 300 block-size))]
    [[(inc x) (inc y) block-size] (box-power (get-box matrix x y block-size))]))

(def part1 
  (->> (make-matrix 3628)
       (all-boxes-power 3)
       (apply max-key second)
       first
       (take 2)))


(def part2
  (let [matrix (make-matrix 3628)]
    (->> (range 1 301)
         (pmap #(apply max-key second (all-boxes-power % matrix)))
         (apply max-key second))))

  