(ns advent-of-code.2018.day8
    (:require [clojure.string :as str]))
  
(defn license-tree
  [input]
  (letfn [(parse-node [[c-count m-count & more]]
            (let [[c more] (nth (iterate parse-child [[] more]) c-count)
                  [m more] (split-at m-count more)]
              [{:c c :m m} more]))
          (parse-child [[children more]]
                       (let [[child more] (parse-node more)]
                         [(conj children child) more]))]
    (first (parse-node input))))

(def input (->> (slurp "resources/2018/input8")
                (re-seq #"\d+")
                (map #(Integer/parseInt %))))

(def part1 (->> (license-tree input)
                (tree-seq #(not-empty (:c %)) :c)
                (mapcat :m)
                (apply +)))

(defn node-val
  [{:keys [c m] :as n}]
  (cond
    (not n) 0
    (empty? c) (apply + m)
    :else (reduce
           (fn [sum m]
             (+ sum (node-val (get c (dec m)))))
           0
           m)))

(def part2 (->> (license-tree input)
                node-val))
