(ns advent-of-code.2018.day7
    (:require [clojure.string :as str]))

(def regex #"Step ([A-Z]) .*? step ([A-Z]) .*")

(defn build-graph-with-dependency-count
  [input]
  (reduce 
   (fn [[g d] [_ f t]]
     (let [from (get g f #{})
           dependency (get d t 0)]
       [(assoc g f (conj from t))
        (assoc (if (not (contains? d f)) (assoc d f 0) d)
               t (inc dependency))]))
   [{} {}]
   input))

(defn finish-dependency
  [graph dep-count from]
  (let [deps (graph from) ]
    (-> (reduce 
         (fn [dep-count dep] (update dep-count dep dec))
         dep-count 
         deps)
        (dissoc from))))

(defn free-nodes
  [dependency-count]
  (->> 
   (filter (fn [[t c]] (zero? c)) dependency-count)
   (map first)))

(def input
  (->> (slurp "resources/2018/input7")
        (str/split-lines)
       #_ test-input
       (map #(re-find regex %))
       (build-graph-with-dependency-count)))

(defn part1 [[g d]]
  (loop [available (sort (free-nodes d))
         dependency-counts d 
         result []]
        (if (empty? available)
          result
          (let [[n & _] available
                d (finish-dependency g dependency-counts n)
                available (sort (free-nodes d))]
               (recur available d (conj result n))))))


(defn make-workers
  [count]
  (zipmap (range 1 (inc count)) (repeat {:assigned nil :time-required 0})))

(defn get-free-worker-ids
  [workers]
  (->> (filter (fn [[_ state]] (not (:assigned state)) ) workers)
     (map first)))

(defn get-assigned-workers
  [workers]
  (->> (filter (fn [[_ state]] (:assigned state) ) workers)
     (map first)))

(defn do-work
  [workers]
  (let [assigned-workers (filter (fn [[_ state]]   (not-empty (:assigned state))) workers)
        [_ {:keys [time-required]}] (when (not-empty assigned-workers) (apply min-key :time-required assigned-workers)) ]

    (if time-required
      [time-required
       (->> (filter (fn [[_ state]] (= time-required (:time-required state))) assigned-workers)
           (mapv (fn [[_ {:keys [assigned]}]] assigned))) 
       (->>
        (filter (fn [[_ state]] (= time-required (:time-required state))) assigned-workers)
        (reduce (fn [w [id _]] (assoc w id {:assigned nil :time-required 0})) workers)
        (map (fn [[id {:keys [assigned time-required] :as v}]]
               (if assigned
                 [id {:assigned assigned :time-required time-required}]
                 [id v])))
        (into {}))]
      [0 [] workers])))

(def task-cost (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 1 27)))

(defn assign-worker
  [workers id task time-required]
  (assoc workers id {:assigned task :time-required time-required}))


(defn part2 [[g d]]
  (loop [available (sort (free-nodes d))
         dependency-counts d 
         workers (make-workers 2)
         result 0]
        (if (empty? available)
          result
          (let [free-workers (get-free-worker-ids workers)
              workers-to-assign (take (count free-workers) available)
              assigned-workers (reduce (fn [w [id task]] (assign-worker w id task (task-cost task))) workers (map workers-to-assign available))
              [time-elapsed tasks-accomplished workers] (do-work assigned-workers)
                d (reduce (fn [d t] (finish-dependency g d t)) dependency-counts tasks-accomplished)
                available (sort (free-nodes d))]
               (recur available d workers (+ result time-elapsed))))))


(def test-input [
"Step C must be finished before step A can begin."
"Step C must be finished before step F can begin."
"Step A must be finished before step B can begin."
"Step A must be finished before step D can begin."
"Step B must be finished before step E can begin."
"Step D must be finished before step E can begin."
"Step F must be finished before step E can begin."])

(re-find regex "Step B must be finished before step K can begin.")

(free-nodes (second input))
(finish-dependency "C" (first input) (second input))

(do-work (assign-worker (make-workers 5) 1 "C" 2))