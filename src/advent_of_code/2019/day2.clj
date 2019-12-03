(ns advent-of-code.2019.day2
  (:require [com.rpl.specter :as s]
            [clojure.string :as str]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #",")
    (s/transform [s/ALL] #(Integer/parseInt %) $)))


(defn apply-op
  [op opr1 opr2 state]
  (let [ operand1 (get state opr1) 
        operand2 (get state opr2)]
    (if (= op 1)
      (+ operand1 operand2)
      (* operand1 operand2))))

#_(apply-op 2 0 0 [2 0 0 0])

(defn compute
  [[offset state _]]
  (let [op                 (get state offset)
        [op opr1 opr2 res] (if (not (= op 99)) (first (s/select (s/srange offset (+ offset 4)) state) ) [op 0 0 0])]
    (if (= op 99)
      [offset state true]
      [(+ offset 4) (assoc state res (apply-op op opr1 opr2 state)) false])))

#_(compute [0 [1 0 0 0]])
#_(compute [8 [1,9,10,3,2,3,11,0,99,30,40,50]])

(defn computer
  [state]
  (->> (iterate compute [0 state false])
       (take-while (fn [[_ _ terminated]] (not terminated)))
       last))

#_(computer [1,9,10,3,2,3,11,0,99,30,40,50])

;;part 1
(-> (read-input "resources/2019/input2")
    (assoc 1 12)
    (assoc 2 2) 
    (computer))

;;part 2
(def input (-> (read-input "resources/2019/input2")
    (assoc 1 12)
    (assoc 2 2)))

(for [noun (range 0 (count input))
      verb (range 0 (count input))
      pos (range 1 (count input) 4)
      :let [start-state (-> (assoc input pos noun) (assoc (inc pos) verb))
            computed (computer start-state)]
      :when (= 19690720 (first (get computed 1)))]
  [noun verb])
