(ns advent-of-code.2021.day10
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (-> file slurp str/split-lines))

(defn evaluate-line
  ([line]
   (evaluate-line (seq line) nil))
  ([[f & r] stack]
   (let [top (first  stack)
         brackets {\} \{ \] \[ \) \( \> \<}
         scores {\} 1197 \] 57 \) 3 \> 25137}]
     (cond
       (not f) [0 stack]
       (and (contains? brackets f) (not= top (brackets f))) [(scores f) stack]
       (and (contains? brackets f) (= top (brackets f)))  (recur r (rest stack))
       :else (evaluate-line r (cons f stack))))))

(def input (read-input "resources/2021/input10"))
;10.1
(->> input ;; => 215229
     (map (comp first evaluate-line))
     (apply +))
;10.2
(defn new-score
  [stack]
  (let [scores {\{ 3 \[ 2 \( 1 \< 4}]
    (reduce
     (fn [score c] (+ (* 5 score) (scores c)))
     0
     stack)))

(->> input ;; => 1105996483
     (map evaluate-line)
     (filter (fn [[score stack]] (and (zero? score) (seq stack))))
     (map (comp new-score second))
     (sort)
     ((fn [scores] (nth scores (/ (count scores) 2)))))
