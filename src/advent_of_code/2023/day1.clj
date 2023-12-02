(ns advent-of-code.2023.day1
  (:require [clojure.string :as str]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #"\n")))

(def digit-map {"one" "1"
                   "two" "2"
                   "three" "3"
                   "four" "4"
                   "five" "5"
                   "six" "6"
                   "seven" "7"
                   "eight" "8"
                   "nine" "9"})

(def digit-regex 
  (as-> digit-map $
    (keys $)
    (str/join "|" $)
    (str "(?=(\\d|" $ "))")
    (re-pattern $)))

(defn ->digit
  [s]
  (or (get digit-map s) s))

(defn digits
  [string]
  (->> string (re-seq digit-regex) (map (comp ->digit last))))

(defn ->number
  [s]
  (let [f (Integer/parseInt (first s))
        l (Integer/parseInt(last s))]
    (+ (* 10 f) l)))

;;part-1
(->> (read-input "resources/2023/input1")
     (map #(re-seq #"\d" %))
     (map ->number)
     (apply +)) ; 54877

(->> (read-input "resources/2023/input1")
     (map digits)
     (map ->number)
     (apply +)) ; 54100
