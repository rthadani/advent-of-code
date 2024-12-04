(ns advent-of-code.2024.day4
  (:require [clojure.string :as str]))

(defn read-input
  [lines]
  (->> lines
       (str/split-lines)
       (mapv #(vec (seq %)))))

(def input (read-input (slurp "resources/2024/input4")))

(defn ew
  [mat row col num-chars op]
  (apply str 
         (for [i (range num-chars)
               :when (and (>= (op col i) 0) (< (op col i) (count (mat 0))))]
           (get-in mat [row (op col i)]))))
(ew input 0 0 4 +)
(ew input 0 0 4 -)

(defn no-so
  [mat row col num-chars op]
  (apply str 
         (for [i (range num-chars)
               :when (and (>= (op row i) 0) (< (op row i) (count mat)))]
           (get-in mat [(op row i) col]))))
(no-so input 0 0 4 +)
(no-so input 4 0 4 -)

(defn diagonal
  [mat row col num-chars row-op col-op]
  (apply str 
         (for [i (range num-chars )
               :when (and (>= (col-op col i) 0) (< (col-op col i) (count (mat 0)))
                          (>= (row-op row i) 0) (< (row-op row i) (count mat))) ]
           (get-in mat [(row-op row i) (col-op col i)]))))
(diagonal input 0 0 4 + +)
(diagonal input 3 4 4 - -)


(defn xmases
  [mat row col]
  (let [straight-ops (map #(partial % mat row col 4) [ew ew no-so no-so]) 
        diagonal-ops (repeat 4 (partial diagonal mat row col 4))
        straight-calcs [+ - + -]
        diagonal-calcs [[- -] [- +] [+ +] [+ -]]]
    (prn "Calculating at pos " row col)
    (+ 
      (apply + (pmap (fn [op calc] #_(prn (op calc)) (if (= "XMAS" (op calc)) 1 0)) straight-ops straight-calcs)) 
      (apply + (pmap (fn [op calc] #_(prn (apply op calc))(if (= "XMAS" (apply op calc)) 1 0)) diagonal-ops diagonal-calcs)))))

(defn x-mases
  [mat row col]
  (let [diagonal-ops (repeat 2 (partial diagonal mat))
        diagonal-calcs [[(dec row) (dec col) 3 + +] [(dec row) (inc col) 3 + -]]]
    (prn "Calculating at pos " row col)
    (->> 
        (pmap (fn[op calc] (sort (apply op calc))) diagonal-ops diagonal-calcs) 
        (every? (fn [diag] (= diag [\A \M \S])))
        ((fn [r] (if (true? r) 1 0))))))

;1
(apply + (for [i (range (count input))
      j (range (count (input 0)))
      :when (= \X (get-in input [i j]))]
  (xmases input i j))) ; 2427
;2
(apply + (for [i (range (count input))
      j (range (count (input 0)))
      :when (= \A (get-in input [i j]))]
  (x-mases input i j))) ; 1900

