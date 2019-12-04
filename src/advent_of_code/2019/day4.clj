(ns advent-of-code.2019.day4
 (:require [com.rpl.specter :as s]
            [clojure.string :as str]))


(defn valid-password?
  [password]
  (let [pairs (->> password str (partition 2 1))]
    (and (every? (fn [[f s]] (>= (int s) (int f))) pairs)
        (some (fn [[f s]] (= f s)) pairs))))

(valid-password? 123444)

(defn two-repeated [value]
  (->> (re-seq #"(\d)\1+" (str value))
       (map first)
       (some #(= 2 (count %)))
       (some?)))

(re-seq #"(\d)\1+" (str 111122))

(defn valid-password-2?
  [password]
  (and (valid-password? password)
       (two-repeated password)))
(valid-password-2? 122444)

;;part 1
(count (filter valid-password? (range 152085 670284)))
(count (filter valid-password-2? (range 152085 670284)))
