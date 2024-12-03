(ns advent-of-code.2024.day2
  (:require [clojure.string :as str]))


(defn read-input
  [input]
  (->> (str/split-lines input)
       (reduce (fn [levels l] 
                 (let [level (str/split l #"\s+")]
                   (conj levels (mapv #(Integer/parseInt (str/trim %)) level)))) 
               [])))

(def input (->> "resources/2024/input2" slurp read-input) )

(defn all-increasing-or-decreasing? 
  [diffs]
  (or (every? #(< % 0) diffs) (every? #(> % 0) diffs)))

(defn within-range?
  [diffs]
  (every? #(<= 1 (abs %) 3) diffs))

(defn ordered-and-within-range?
  [level]
  (let [partitioned (partition 2 1 level)]
  (as-> partitioned $
       (map #(- (first %) (second %)) $)
       (and (all-increasing-or-decreasing? $) (within-range? $)))))

(defn can-fix-level?
  [level]
  (or (ordered-and-within-range? level)
      (some ordered-and-within-range?
            (map #(into (subvec level 0 %) (subvec level (inc %)))(range (count level))))))

;;1
(->> input (filter ordered-and-within-range?) count) ; 483

;;2
(->> input (filter #(can-fix-level? %)) count)  ; 528
