(ns advent-of-code.2018.day4
    (:require [clojure.string :as str]))

(def regex #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (Guard #(\d+) begins shift)?(wakes up)?(falls asleep)?")

(defn parse-line
  [line]
  (let [[_ year month day hour minute begin-shift? guard-id wake? sleep?] (re-find regex line)]
    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :event-type (cond begin-shift? :begin-shift wake? :wake-up sleep? :sleep)
     :id  (if begin-shift? (Integer/parseInt guard-id) nil)}))

(parse-line "[1518-08-17 00:03] Guard #1597 begins shift")
(parse-line "[1518-10-06 00:47] wakes up")
(parse-line "[1518-10-06 00:47] falls asleep")

(defn copy-ids
  [sorted-log]
  (first
   (reduce
    (fn [[result current-id] log]
      (if (:id log) 
        [(conj result log) (:id log)]
        [(conj result (assoc log :id current-id)) current-id]) )
    [[] nil]
    sorted-log)))

(def event-log
  (->> (slurp "resources/2018/input4")
       (str/split-lines)
       (map parse-line)
       (sort-by (juxt :year :month :day :hour :minute))
       (copy-ids)))


(defn- get-sleep-time
  [[id all-events]]
  [id (count (filter #(= (:event-type %) :sleep) all-events))])

(defn highest-sleeper-id
  []
  (->> (group-by :id event-log)
     (map get-sleep-time)
     (sort-by second)
     last
     first))

(highest-sleeper-id)


(def part2
  (let [sleeper (highest-sleeper-id)
        minute (->> event-log
                    (filter #(and (= (:id %) sleeper) (= (:event-type %) :sleep)))
                    (group-by (juxt :hour :minute))
                    (sort-by (fn [[_ v]] (count v)))
                    last
                    first
                    second)]
       (* sleeper minute)))


(count event-log)
(take 10 event-log)
(first event-log)
(last event-log)




