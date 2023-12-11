(ns advent-of-code.2023.day8
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (let [[direction nodes] (-> file slurp (str/split #"\n\n"))
        _ (prn direction nodes)
        nodes (->> nodes 
                   (str/split-lines) 
                   (reduce (fn [n l] 
                             (let [dirs (re-seq #"[A-Z]+" l)]
                               (conj n [(first dirs) (vec (rest dirs))]))) 
                           []))]
    [direction nodes]))

(defn get-val
  [nodes key]
  (->> (filter #(= (first %) key) nodes)
       second))

(let [[d n] (read-input "resources/2023/input8")]
  (reduce 
    (fn [[k i] d]
      (if (= k "ZZZ")
        (reduced i)
       (case d
        \L [ (inc i)])))
    [(ffirst n) 0]
    d))
