(ns advent-of-code.2021.day14
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (let [[polymer transforms] (-> (slurp file) (str/split #"\n\n"))
        transforms (->> transforms str/split-lines
                        (map #(str/split % #" -> "))
                        (map (fn [[[a b] c]] [(str a b) [(str a c) (str c b)]]))
                        (into {}))]
    {:polymer (->> polymer (partition 2 1) (map str/join) frequencies)
     :transforms transforms
     :frequencies (frequencies polymer)}))

(def input (read-input "resources/2021/input14"))

(defn char-frequencies
  [{:keys [polymer transforms frequencies] :as input}]
  (->> polymer
       (map (fn [[pair f]] [(-> transforms (get pair) first second) f]))
       (reduce (fn [f [c n]] (update f c (fnil + 0) n)) frequencies)))

(defn grow-polymer
  [{:keys [polymer transforms] :as input}]
  (->> polymer
       (mapcat (fn [[pair n]] (map (fn [t] [t n]) (transforms pair))))
       (reduce (fn [r [p f]] (update r p (fnil + 0) f)) {})))

(defn step
  [{:keys [transforms] :as input}]
  {:polymer (grow-polymer input)
   :transforms transforms
   :frequencies (char-frequencies input)})

(defn answer
  [input steps]
  (->> input
       (iterate step)
       (drop steps)
       first
       :frequencies
       (sort-by second)
       ((juxt last first))
       (map second)
       (apply -)))

;;14.1
(answer input 10) ;; => 2851
;;14.2
(answer input 40) ;; => 10002813279337
