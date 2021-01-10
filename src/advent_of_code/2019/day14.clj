(ns advent-of-code.2019.day14
  (:require [clojure.string :as str]))


(defn quantity-and-material
  [input]
  (let [[quantity material] (-> (str/trim input) (str/split #"\s"))]
    {:quantity (Integer/parseInt quantity)
     :material material}))

(defn create-lhs-rhs
  [[lhs rhs]]
  [(quantity-and-material rhs)
   (map quantity-and-material (str/split lhs #","))])

(defn read-input 
  [input]
  (->> (slurp input)
      (str/split-lines)
      (map #(str/split % #"=>"))
      (map create-lhs-rhs)))


(defn find-rhs 
  [quantity-and-material right]
  (filter (fn [[rhs _]] (= (:material rhs) right)) quantity-and-material))

(defn find-ore 
  [quantity-and-material]
  (filter (fn [[_ lhs]] (some #(= % "ORE") (mapv :material lhs))) quantity-and-material))

(def input (read-input "resources/2019/input14"))

#_ (find-rhs input "ORE")
#_ (find-ore input)
#_ (contains? (mapv :material '({:quantity 2, :material "CLML"} {:quantity 4, :material "SWMTK"} {:quantity 16, :material "ORE"}) ) "ORE")