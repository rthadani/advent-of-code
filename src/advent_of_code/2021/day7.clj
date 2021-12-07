(ns advent-of-code.2021.day7
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (mapv #(Integer/parseInt %) (-> (slurp file) (str/replace #"\n" "") (str/split #","))))

(def input (read-input "resources/2021/input7"))

(defn fuel-distance
  [from-points to-point]
  (for [i from-points]
    (Math/abs (- i to-point))))

(defn fuel-num-steps
  [from-points to-point]
  (map #(/ (* % (inc %)) 2) (fuel-distance from-points to-point)))

(defn min-fuel [distance-fn from-points to-points]
  (->> to-points
       (map #(apply + (distance-fn from-points %)))
       (apply min)))

;; 7.1
(min-fuel fuel-distance input input)  ;; => 355521
;; 7.2
(min-fuel fuel-num-steps input (vec (range (apply min input) (inc (apply max input))))) ;; => 100148777
