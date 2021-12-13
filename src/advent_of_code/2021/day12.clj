(ns advent-of-code.2021.day12
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn read-input [file]
  (->> (slurp file)
       str/split-lines
       (map #(str/split % #"-"))
       (map (fn [[k v]] {k #{v} v #{k}}))
       (apply merge-with set/union)))

(def input (read-input "resources/2021/input12"))

(defn small-cave? [n] (= n (str/lower-case n)))

(defn add-node-to-path?
  [path node repeats]
  (if (and (not= node "start") (small-cave? node))
    (let [path (conj path node)
          small-caves (filter small-cave? path)]
      (<= (count small-caves) (+ (count (set small-caves)) repeats)))
    (not= node "start")))

(defn passage-paths [graph repeats]
  (letfn [(visit [path s]
            (let [path* (conj path s)
                  dests (filter #(add-node-to-path? path* % repeats) (graph s))]
              (if (= s "end")
                [path*]
                (mapcat #(visit path* %) dests))))]
    (->> (graph "start")
         (mapcat #(visit ["start"] %))
         count)))

;;12.1
(passage-paths input 0) ;; => 3761

;;12.2
(passage-paths input 1) ;; => 99138
