(ns advent-of-code.2018.day7
    (:require [clojure.string :as str]))

(def regex #"Step ([A-Z]) .*? step ([A-Z]) .*")

(re-find regex "Step B must be finished before step K can begin.")

(defn build-graph-with-dependency-count
  [input]
  (reduce 
   (fn [[g d] [_ f t]]
    (println g d f t )
     (let [from (get g f #{})
           dependency (get d t 0)]
       [(assoc g f (conj from t))
        (assoc (if (not (contains? d f)) (assoc d f 0) d)
               t (inc dependency))]))
   [{} {}]
   input))

(def test-input [
"Step C must be finished before step A can begin."
"Step C must be finished before step F can begin."
"Step A must be finished before step B can begin."
"Step A must be finished before step D can begin."
"Step B must be finished before step E can begin."
"Step D must be finished before step E can begin."
"Step F must be finished before step E can begin."])
(build-graph-with-dependency-count test-input)
(free-nodes )

(defn finish-dependency
  [from graph dep-count]
  (let [deps (graph from) ]
     (reduce 
      (fn [dep-count dep] (update dep-count dep dec))
      dep-count 
      deps)))

(defn free-nodes
  [dependency-count]
  (->> 
   (filter (fn [[t c]] (zero? c)) dependency-count)
   (map first)))

(def input
  (->> #_(slurp "resources/2018/input7")
       #_ (str/split-lines)
       test-input
       (map #(re-find regex %))
       (build-graph-with-dependency-count)))


(defn part1 [[g d]]
  (loop [available (sort (free-nodes d))
         dependency-counts d 
         result []]
        (if (empty? available)
          result
          (let [[n & rest] available
                d (finish-dependency n)
                available (sort (free-nodes d))]
               (recur available d (conj result n)))))

(part1 (build-graph-with-dependency-count input))
