(ns advent-of-code.2021.day15
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]))

(defn build-graph
  [matrix]
  (->> (for [i (range 0 (count matrix))
             j (range 0 (count (matrix 0)))]
         [[i j] (get-in matrix [i j])])
       (into {})))

(def cave-risk
  (memoize (fn [n] (nth (cons 0 (cycle (range 1 10))) n))))

(defn multiply-caves [cave n]
  (let [length (inc (apply max (map ffirst cave)))]
    (apply merge (for [grid-x (range 0 n)
                       :let [x-offset (* grid-x length)]
                       grid-y (range 0 n)
                       :let [y-offset (* grid-y length)
                             p-offset [x-offset y-offset]
                             n-offset (+ grid-x grid-y)]]
                   (reduce (fn [acc [p n]] (assoc acc (mapv + p p-offset)
                                                  (cave-risk (+ n n-offset))))
                           {} cave)))))

(defn read-input [file multiples]
  (let [matrix (->> (slurp file) (str/split-lines) (mapv #(mapv (fn [i] (Integer/parseInt (str i))) (seq %))))]
    {:graph  (multiply-caves (build-graph matrix) multiples)
     :max-x (* multiples (count matrix))
     :max-y (* multiples (count (matrix 0)))}))

(def input (read-input "resources/2021/input15" 1))

(defn neighbors
  [[x y] {:keys [graph max-x max-y]}]
  (->> (for [[ox oy] [[0 -1] [0 1] [-1 0] [1 0]]
             :when (and (>= (+ x ox) 0)
                        (>= (+ y oy) 0)
                        (< (+ x ox) max-x)
                        (< (+ y oy) max-y))]
         [(+ x ox) (+ y oy)])
       (select-keys graph)))

(defn map-vals [nodes f]
  (into {} (for [[k v] nodes] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  [{:keys [graph max-x max-y] :as input}]
  (loop [q (priority-map [0 0] 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (neighbors v input) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))
;15.1
(get (dijkstra input) [(dec (:max-x input)) (dec (:max-y input))]) ;; => 581

;15.2
(def input (read-input "resources/2021/input15" 5))

(get (dijkstra input) [(dec (:max-x input)) (dec (:max-y input))]) ;; => 2916
