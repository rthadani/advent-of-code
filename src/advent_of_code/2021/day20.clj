(ns advent-of-code.2021.day20
  (:require [clojure.string :as str]))

(defn read-input
  [file]
  (let [[a i] (-> file slurp (str/split #"\n\n"))
        algorithm (mapv identity (str/replace a #"\n" ""))
        image (mapv #(mapv identity %) (str/split-lines i))]
    {:algorithm algorithm :image image}))

(def input (read-input "resources/2021/input20"))

(defn pixel-value
  [mat algo x y]
  (-> (for [ox [-1 0 1] oy [-1 0 1]]
        (if (= (get-in mat [(+ x ox) (+ oy y)] \.) \#) 1 0))
      (str/join)
      (Integer/parseInt 2)
      (algo)))

(defn grow-image
  [image]
  (vec (for [i (range -1 (inc (count image)))]
         (vec (for [j (range -1 (inc (count image)))]
                (get-in image [i j]))))))

(defn enhance-image
  [algorithm image]
  (let [next-image (grow-image image)]
    (vec (for [x (range 0 (count next-image))]
           (vec (for [y (range (count (next-image 0)))]
                  (pixel-value next-image algorithm x y)))))))

(defn count-lit
  [image]
  (->> image flatten (filter #(= % \#)) count))

(defn enhance-steps
  [steps]
  (->> (:image input)
       (iterate (partial enhance-image (:algorithm input)))
       (drop steps)
       first
       (count-lit)))
;20.1
(enhance-steps 2) ;; => 5419
;20.2
(enhance-steps 50) ;; => 17325
