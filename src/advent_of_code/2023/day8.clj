(ns advent-of-code.2023.day8
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (let [[direction nodes] (-> file slurp (str/split #"\n\n"))
        _ (prn direction nodes)
        nodes (->> nodes 
                   (str/split-lines) 
                   (reduce (fn [n l] 
                             (let [dirs (re-seq #"[A-Z\d+]+" l)]
                               (conj n [(first dirs) (vec (rest dirs))]))) 
                           [])
                  (into {}))]
    [direction nodes]))

(defn travel
  [directions nodes starting-pos]
   (reductions (fn [curr dir]
                 (if (= dir \L) 
                   (first (nodes curr))
                   (second (nodes curr))))
               starting-pos
               (cycle directions)))

(defn step-count
  [start directions end-when nodes]
  (->> start 
       (travel directions nodes)
       (take-while end-when)
       count))
;;part 1
(let [[d n] (read-input "resources/2023/input8")]
  (step-count "AAA" d (complement #{"ZZZ"}) n)) ; 18673

;;part-2
(defn lcm
  [a n]
  (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
          (/ (* a n) (gcd a n))))

(let [[d n] (read-input "resources/2023/input8")]
  (->> (keys n)
       (filter (comp #{\A} last))
       (map (fn [s] (step-count s d (comp (complement #{\Z}) last) n)))
       (reduce lcm 1)) ) ; 17972669116327

