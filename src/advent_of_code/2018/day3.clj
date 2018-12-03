(ns advent-of-code.2018.day3
    (:require [clojure.string :as str]))


(def line-regex #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn make-claim
  [line]
  (let [parts (rest (re-find line-regex line))
        [id l t w h] (map #(Integer/parseInt %)  parts)]
    {:id id :l l :t t :r (+ l w) :b (+ h t)}))

(defn mark-claim
  [claim]
  (for [i (range (:l claim) (:r claim))
        j (range (:t claim) (:b claim))]
    [i j]))

(def input 
  (-> (slurp "resources/2018/input3")
    (str/split-lines)))

(def part1 
  (->> input
    (mapcat (comp mark-claim make-claim))
    frequencies
    vals
    (filter #(>= % 2))
    count))

(def part2-claims 
  (->> input 
    (map make-claim) ))

(defn find-id-with-xy
  [all-coords-with-1]
  (let [cords (into #{} (map first all-coords-with-1))
        all-cords-for-claim (fn [{:keys [l t r b]}]
                            (for [i (range l r)
                                  j (range t b)]
                                  [i j]))]
      (filter
       (fn [claim]
         (every? #(contains? cords %) (all-cords-for-claim claim) )) 
       part2-claims)))

(def part2
  (->> part2-claims
       (mapcat mark-claim)
       frequencies
       (filter (fn [[coords freq]] (= freq 1)))
       (find-id-with-xy)))
