(ns advent-of-code.2021.day8
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(defn read-input
  [file]
  (as-> (slurp file) $
    (str/replace $ "|"  "_")
    (str/split-lines $)
    (map #(str/split % #"\s_\s") $)))

(def input (read-input "resources/2021/input8"))

(defn get-digit
  [patterns display]
  (cond
    (= 2 (count display)) 1
    (= 3 (count display)) 7
    (= 4 (count display)) 4
    (= 7 (count display)) 8
    :else (->> (set display) (vec) sort (str/join) patterns)))

;;8.1
(->> input ;; => 512
     (mapcat (fn [[_ o]]  (str/split o #"\s")))
     (map (partial get-digit {}))
     (frequencies)
     ((fn [r] (select-keys r [1 4 7 8])))
     vals
     (apply +))

;; 8.2
(defn make-patterns
  [inputs]
  (let [strings-of-length (fn [n]  (filter #(= n (count %)) inputs))
        contains-str? (fn [a b]  (every? #((set a) %) b)) ;a contains b
        make-key (fn [s] (->> (set s) (vec) sort (str/join)))
        one (first (strings-of-length 2))
        seven (first (strings-of-length 3))
        four (first (strings-of-length 4))
        eight (first (strings-of-length 7))
        six (first (filter #(not (contains-str? % seven)) (strings-of-length 6)))
        nine (first (filter #(contains-str? % four) (strings-of-length 6)))
        five (first (filter #(contains-str? six %) (strings-of-length 5)))
        two (first (filter #(not (contains-str? nine %)) (strings-of-length 5)))
        three (first (filter #(not (#{two five} %)) (strings-of-length 5)))
        zero (first (filter #(not (#{six nine} %)) (strings-of-length 6)))]
    {(make-key zero) 0
     (make-key two) 2
     (make-key three) 3
     (make-key five) 5
     (make-key six) 6
     (make-key nine) 9}))

(defn get-digit-2
  [[input output]]
  (let [patterns (make-patterns input)
        [a b c d] (map (partial get-digit patterns) output)]
    (+ d (* c 10) (* b 100) (* a 1000))))

(->> input ;; => 1091165
     (map (fn [[i o]]  [(str/split i #"\s") (str/split o #"\s")]))
     (map get-digit-2)
     (apply +))
