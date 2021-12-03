(ns advent-of-code.2021.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input
  [file]
  (->> (slurp file)
       (str/split-lines)
       (mapv #(vec (str/split % #"")))))

(def input (read-input "resources/2021/input3"))

(defn counts
  [c m]
  (->> (map #(nth % c) m)
       (group-by identity)
       (map (fn [[k vs]] [k (count vs)]))
       (into {})))

#_(counts 0 input)

(defn ge
  [col]
  (if (> (col "0") (col "1"))
    ["0" "1"]
    ["1" "0"]))

;; 3.1
(->> (range 0 (count (input 0))) ;; => 3429254
     (map #(counts % input))
     (reduce
      (fn [[g e] col-counts]
        (let [[cg ce] (ge col-counts)] [(str g cg) (str e ce)]))
      ["" ""])
     (map #(Integer/parseInt % 2))
     (apply *))

;;3.2
(defn select-rows
  [m c o2?]
  (let [{zs "0" ones "1" :or {zs 0 ones 0} :as cs} (counts c m)
        select-col (cond
                     (> zs ones) (if o2? "0" "1")
                     (< zs ones) (if o2? "1" "0")
                     o2? "1"
                     :else "0")]
    (vec (filter #(= select-col (nth % c)) m))))
#_(count (select-rows input 0 false))

(defn get-last-row
  [m o2?]
  (reduce
   (fn [[m i] _] (let [newm (select-rows m i o2?)]
                   (if (= 1 (count newm))
                     (reduced (str/join (first newm)))
                     [newm (inc i)])))
   [m 0]
   (range 0 (count m))))

(->> [(get-last-row input true) (get-last-row input false)] ;; => 5410338
     (map #(Integer/parseInt %  2))
     (apply *))
