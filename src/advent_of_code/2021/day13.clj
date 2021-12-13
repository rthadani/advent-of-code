(ns advent-of-code.2021.day13
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (let [[cs fs] (-> (slurp file) (str/split #"\n\n"))
        coords (map (fn [l] (mapv #(Integer/parseInt %) (str/split l #","))) (str/split-lines cs))
        r #"^fold\salong\s(x|y)=(\d+)$"
        folds (map (fn [l] (let [[_ axis v] (re-find (re-matcher r l))] [(keyword axis) (Integer/parseInt v)])) (str/split-lines fs))]
    {:coords coords
     :folds folds}))

(def input (read-input "resources/2021/input13"))

(defn fold
  [coords c x?]
  (let [before-fold (filter (fn [[x y]] (if x? (< x c) (< y c))) coords)
        after-fold (filter (fn [[x y]] (if x? (> x c) (> y c))) coords)]
    (reduce
     (fn [result [x y]] (conj result (if x? [(- c (- x c)) y] [x (- c (- y c))])))
     (set before-fold)
     after-fold)))

;;13.1
(count (fold (:coords input) (second (first (:folds input))) true)) ;; => 618

;;13.2
(defn print-letters
  [coords]
  (doseq [i (range 0 (inc (second (apply max-key second coords))))]
    (doseq [j (range 0 (inc (first (apply max-key first coords))))]
      (if (contains? coords [j i])
        (print "#")
        (print " ")))
    (println)))

(->> (reduce ;; => ALREKFKU
      (fn [c [k v]] (fold c v (= :x k)))
      (:coords input)
      (:folds input))
     (print-letters))
