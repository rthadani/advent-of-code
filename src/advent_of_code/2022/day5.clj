(ns advent-of-code.2022.day5
  (:require [clojure.string :as str]))

(defn make-stacks
  [stacks]
  (let [stacks (butlast (str/split-lines stacks))
       match-lines #"([\[\s]?[A-Z\s]?[\]\s]?)+"  ]
    (->> stacks 
         (map #(->> % (partition-all 4) (mapv  (fn [e] (subs (str/join "" e) 1 2)))))
         (reduce (fn [a r] 
                   (first (reduce (fn [[a i] e] (if (empty? (str/trim e)) [a (inc i)] [(assoc a i (conj (nth a i) e)) (inc i)])) 
                           [a 0] r))) 
                 (vec (repeat 9 []))))))

(defn make-moves
  [moves]
  (let [regex #"move (\d+) from (\d) to (\d)"]
  (->> (str/split-lines moves)
      (mapv (fn [m] (-> (re-matches regex m) rest)) ))))

(defn read-input
  [file]
  (let [all-data (slurp file)
        [stacks moves] (str/split all-data  #"\n\n")]
    {:stacks (make-stacks stacks)
     :moves (make-moves moves)}))

(defn move-elements
  [[how-many from to] stacks part1]
  (let [how-many (Integer/parseInt how-many)
        from (dec (Integer/parseInt from))
        to (dec (Integer/parseInt to)) 
        reverse (if part1 reverse identity) ]
    (-> stacks
        (assoc from (subvec (stacks from) how-many))
        (assoc to (vec (concat (reverse (subvec (stacks from) 0 how-many)) (stacks to)))))))

;; part1
(let [{:keys [stacks moves]} (read-input "resources/2022/input5")]
     (->> moves
          (reduce (fn [s m] (move-elements m s true)) stacks)
          (map first)
          (str/join ""))) ; "GFTNRBZPF"

;; part2
(let [{:keys [stacks moves]} (read-input "resources/2022/input5")]
     (->> moves
          (reduce (fn [s m] (move-elements m s false)) stacks)
          (map first)
          (str/join "")))  ; "VRQWPDSGP"
