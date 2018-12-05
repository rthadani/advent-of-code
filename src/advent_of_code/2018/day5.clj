(ns advent-of-code.2018.day5
    (:require [clojure.string :as str]))


(defn opposites?
  [a b]
   (or (= (- (int a) (int \a)) (- (int b) (int \A)))
      (= (- (int b) (int \a)) (- (int a) (int \A)))))

(defn skip-char
  [char current-letter]
  (and (not (nil? char))
       (= (.toUpperCase (str char)) (.toUpperCase (str current-letter)))))

(defn alchemy 
  [s & [skip]]
  (reduce
   (fn [r l]
     (cond
       (skip-char skip l) r
       (empty? r)  [l]
       (opposites? l (get r (dec (count r)))) (subvec r 0 (dec (count r))) ;;last and butlast cause linear searches and all sorts of problems
       :else (conj r l)))
   []
   s))

 (count (alchemy "dabAcCaCBAcCcaDA" \d)) 
 (count (alchemy "abBA")) 

(def input 
  (memoize (fn []  (slurp "resources/2018/input5"))))

(def part1 (count (alchemy (input))))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def part2 (->> (char-range \a \z)
             (pmap #(alchemy (input) %))
             (map count)
             (apply min)))