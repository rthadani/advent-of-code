(ns advent-of-code.2024.day3
  (:require [clojure.string :as str]))


(defn find-mult-pairs-1
  [line]
  (let [matcher (re-matcher #"mul\((\d+),(\d+)\)" line)]
  (loop [found (re-find matcher)
         result []]
  (if (nil? found)
    result
    (recur (re-find matcher) 
           (conj result [(Integer/parseInt (second found))(Integer/parseInt (last found))]))))))

(find-mult-pairs-1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn has-dont
  [s dont]
  (cond 
    (str/includes? s "don't()") true
    (str/includes? s "do()") false
    :else dont))

(defn find-mult-pairs-2
  [line]
  (let [matcher (re-matcher #"(don't\(\)|do\(\))?.*?mul\((\d+),(\d+)\)" line)]
  (loop [found (re-find matcher)
         dont false
         result []]
  (if (nil? found)
    result
    (recur (re-find matcher) 
           (has-dont (first found) dont) 
           (if-not (has-dont (first found) dont)
             (conj result [(Integer/parseInt (nth found 2))(Integer/parseInt (last found))])
             result))))))
(find-mult-pairs-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn read-input
  [find-mult-pairs lines]
  (mapcat find-mult-pairs [lines]))


(def input (slurp "resources/2024/input3"))

(defn exec-mult-instructions
  [find-mult-pairs]
(->> 
     input 
     (read-input find-mult-pairs)
     (reduce (fn [a [x y]] (+ a (* x y))) 0))) 
;1
(exec-mult-instructions find-mult-pairs-1) ; 188741603
;2
(exec-mult-instructions find-mult-pairs-2) ; 67269798
