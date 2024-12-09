(ns advent-of-code.2024.day7
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn read-input
  [lines]
  (as-> (str/split-lines lines) $
        (for [line $
              :let [[total & rest] (->> (str/split line #"\:? ") (mapv parse-long))]]
          [total (reverse rest)])))

(def input (read-input (slurp "resources/2024/input7")))

(defn solve-1 
  [total [n & n*]]
  (if (seq n*)
    (or (when (zero? (rem total n)) (solve-1 (quot total n) n*))
        (when (>= total n) (recur (- total n) n*)))
    (= total n)))

;1
(transduce (comp (filter (partial apply solve-1))
                 (map first)) + input) ; 1611660863222

;2
(defn strlen [n] (inc (int (math/log10 n))))

(defn cutoff
  [x y]
  (when (str/ends-with? (str x) (str y))
    (parse-long (subs (str x) 0 (- (strlen x) (strlen y))))))

(defn solve-2
  [total [n & n*]]
  (if (seq n*)
    (or (when (zero? (rem total n)) (solve-2 (quot total n) n*))
        (when (>= total n) (solve-2 (- total n) n*))
        (some-> (cutoff total n) (recur n*)))
    (= total n))) 

(transduce (comp (filter (partial apply solve-2))
                 (map first)) + input) ; 945341732469724
