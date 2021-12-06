(ns advent-of-code.2021.day6
  (:require
   [clojure.string :as str]))

(def file "resources/2021/input6")
(defn read-input
  [file]
  (map #(Integer/parseInt %) (-> (slurp file) (str/replace #"\n" "") (str/split #","))))

(def input (read-input "resources/2021/input6"))

(defn replace-digit
  [digit]
  (cond
    (vector? digit) [(first digit) (replace-digit (second digit))]
    (zero? digit) 6
    :else (dec digit)))

(defn new-fish
  [current-day]
  (reduce (fn [c l]  (if (vector? l)
                       (if (zero? (second l))
                         (+ c (first l))
                         c)
                       (if (zero? l)
                         (inc c)
                         c)))
          0
          current-day))

(defn next-day
  [current-day]
  (let [new-fish (new-fish current-day)
        new-timers (map replace-digit current-day)]
    (if (pos? new-fish)
      (concat new-timers [[new-fish 8]])
      new-timers)))

(defn calculate-fish
  [day]
  (reduce
   (fn [c d] (if (vector? d) (+ c (first d)) (inc c)))
   0
   day))

(defn result
  [n]
  (loop [current-day input
         day 0]
    (if (= day n)
      (calculate-fish current-day)
      (recur (next-day current-day) (inc day)))))

(result 80) ;; => 373378
(result 256) ;; => 1682576647495
