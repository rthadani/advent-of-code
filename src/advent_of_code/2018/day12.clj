(ns advent-of-code.2018.day12
  (:require [clojure.string :as str]
            [advent-of-code.2018.utils :refer [nth']]))

(defn parse-rule
  [result rule]
  (let [[rule pot-state] (str/split rule #" => ")]
    (assoc result rule (nth pot-state 0))))

(defn init-state
  [input]
  (let [[init-state _ & rules] (str/split-lines input)
        pots (subs init-state (count "initial-state: "))]
    {:pots pots 
     :rules (reduce parse-rule {} rules)
     :start 0
     :end (dec (count pots))
     :gen 1}))

(defn apply-rule
  [rules gen]
  (let [rule (get rules (apply str gen))]
    (if rule 
      (->> (concat (take 2 gen) [rule] (drop 3 gen))
      (apply str)))))


(defn next-gen
  [pots rules]
  (->>
   (str "..." pots "...")
   (partition 5 1)
   (map #(apply-rule rules %))
   (map #(nth % 2))
   (apply str)))

(defn live-pots
  [{:keys [pots start end]}]
  (->>
   (zipmap (range start (inc end)) pots)
   (filter (fn [[_ v]] (= v \#)))
   (into {})))

(defn next-state
  [{:keys [pots rules start end gen] :as state}]
  (let [next-g (next-gen pots rules)
        new-start (if (= \# (first next-g)) (dec start)  start)
        new-end (if (= \# (last next-g)) (inc end) end)
        si (if (= start new-start) 1 0)
        ei (if (= end new-end) (dec (count next-g)) (count next-g))]
    (when (zero? (mod gen 10000)) (println state))
    (-> (assoc state :pots (subs next-g si ei))
        (assoc :start new-start)
        (assoc :end new-end)
        (update :gen inc'))))

(defn sum-live-pots
  [generations]
  (as-> (slurp "resources/2018/input12") $
    (init-state $)
    (iterate next-state $)
    (nth' $ generations)
    (live-pots $)
    (keys $)
    (apply + $)))

#_ (def part1 (sum-live-pots 20))
#_ (def part2 (let [n (sum-live-pots 100)
                    p (sum-live-pots 99)]
                (+ (- n p) p (* (- n p) (- 50000000000 100)))))
