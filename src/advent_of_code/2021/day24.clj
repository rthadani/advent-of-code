(ns advent-of-code.2021.day24
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

(defn parse-digit-instruction [lines]
  (letfn [(last-number [n] (->> (lines n) (re-seq #"-?\d+") last read-string))]
    (map last-number [4 5 15])))

(defn read-input [file]
  (->> file
       slurp
       (str/split-lines)
       (partition 18)
       (map (comp parse-digit-instruction vec))))

(defn magic-function [[a b c] z w]
  (if (not= (-> z (mod 26) (+' b)) w)
    (-> z (quot a) (*' 26) (+' w c))
    (quot z a)))

(defn run-instruction-digit
  [z-values-this-round [a b c :as instruction] z [minv maxv] digit]
  (println z-values-this-round)
  (let [new-value-for-z (magic-function instruction z digit)]
    (if (or (= a 1)
            (and (= a 26) (< new-value-for-z z)))
      (assoc z-values-this-round new-value-for-z [(min (get-in z-values-this-round [new-value-for-z 0] Long/MAX_VALUE)
                                                       (-> (*' 10 minv) (+' digit)))
                                                  (max (get-in z-values-this-round [new-value-for-z 1] Long/MIN_VALUE)
                                                       (-> (*' 10 maxv) (+' digit)))])
      z-values-this-round)))

(defn run-instruction [z-values instruction]
  (reduce (fn [z-values-this-round [z min-max]]
            (reduce (fn [zvtr digit] (run-instruction-digit zvtr instruction z min-max digit))
                    z-values-this-round
                    (range 1 3)))
          {}
          z-values))

(defn solve [input]
  (-> (r/reduce (fn [z-values instruction] (run-instruction z-values instruction))
                {0 [0 0]}
                input)
      (get 0)))

(def input (read-input "resources/2021/input24"))

(def answer (solve input))
;;24.1
(second answer) ;=> 41299994879959 
;;24.2
(first answer) ;=> 11189561113216
