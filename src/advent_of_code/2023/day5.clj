(ns advent-of-code.2023.day5
  (:require [clojure.string :as str]))

(defn split-groups [s]
  (str/split s #"\n\n"))

(defn read-input
  [file]
  (->> (slurp file)
       split-groups
       (map str/split-lines)
       (map (partial map #(str/split % #" ")))
       (map (partial map (partial keep #(try (Long/parseLong %) (catch Exception _ nil)))))
       (mapv (partial filterv not-empty))
       ((fn [[seeds & maps]] [(first seeds) maps]))))

(defn ->interval
  [[s l]]
  [s (+ s (dec l))])

(defn intersect [[s1 e1] [s2 e2]]
  (when (<= (max s1 s2) (min e1 e2)) 
    [(max s1 s2) (min e1 e2)]))

(defn subtract-interval
  [[s1 e1] [s2 e2]]
  (when-not (and (<= s2 s1) (<= e1 e2)) 
    (let [[s e] (if (< s1 s2)
                  [s1 (min (dec s2) e1)]
                  [(max s1 (inc e2)) e1])]
      (when-not (< e s)                 
        [s e]))))

(defn intersections
  [source-interval almanac-maps]
  (loop [r [] 
         interval source-interval 
         maps almanac-maps]
    (cond
      (nil? interval) r                
      (empty? maps) (conj r interval)     
      :else
      (let [[[dest source len] & ts] maps 
            other-interval (->interval [source len])]
        (if-let [[s e] (intersect interval other-interval)]
          (recur (conj r (->interval [(+ s (- dest source)) (inc (- e s))]))
                 (subtract-interval interval other-interval)
                 ts)
          (recur r interval ts))))))

;;part1
(let [[seeds almanac-maps] (read-input "resources/2023/input5")
      seeds  (map #(->interval [% 1]) seeds)]
      (->> (reduce (fn[seeds map] 
                     (mapcat #(intersections % map) seeds)) 
                   seeds 
                   almanac-maps)
           (map first)
           (apply min))) ; 313045984

;;part2
(let [[seeds almanac-maps] (read-input "resources/2023/input5")
      seeds  (map ->interval (partition-all 2 seeds))]
      (->> (reduce (fn[seeds map] 
                     (mapcat #(intersections % map) seeds)) 
                   seeds 
                   almanac-maps)
           (map first)
           (apply min)))  ; 20283860

