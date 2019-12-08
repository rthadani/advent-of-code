(ns advent-of-code.2019.day6
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]))


(defn read-input
  [file]
  (->> (slurp file)
       (str/split-lines)
       (s/transform [s/ALL] #(str/split % #"\)"))
       (reduce (fn [coll [v k]] (assoc coll k v)) {})))


(defn orbits-to-center
  [input object-id]
  (->> (iterate input object-id)
       (take-while #(not= % "COM"))))

(def input (read-input "resources/2019/input6"))

;;part 1
(->> (keys input)
     (map #(partial orbits-to-center input))
     (map count)
     (apply +))

;;part 2
(->> (concat (orbits-to-center input "YOU") (orbits-to-center input "SAN"))
     (frequencies)
     (filter #(= (second %) 1))
     (count)
     (+ -2))

