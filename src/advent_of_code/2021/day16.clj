(ns advent-of-code.2021.day16
  (:require [clojure.string :as str :refer [join]]))

(defn read-input
  [file]
  (let [hex-bin {\0 "0000"
                 \1 "0001"
                 \2 "0010"
                 \3 "0011"
                 \4 "0100"
                 \5 "0101"
                 \6 "0110"
                 \7 "0111"
                 \8 "1000"
                 \9 "1001"
                 \A "1010"
                 \B "1011"
                 \C "1100"
                 \D "1101"
                 \E "1110"
                 \F "1111"}]
    (mapcat #(hex->bin %) (slurp file))))
(def input (read-input "resources/2021/input16"))

(defn parse-field
  [bits offset length]

  (Integer/parseInt (str/join (take length (drop offset bits))) 2))

(defn parse-literal
  [bits offset]
  (let [parse-groups (fn [[last? more? offset group]]
                       [(not more?) (not= \0 (nth bits offset)) (+ offset 5) (->> bits (drop offset) (take 5) (drop 1))])
        groups (->> (iterate parse-groups [false true offset []])
                    (take-while #(not (first %)))
                    rest)
        value (->>   groups
                     (mapcat last)
                     (str/join)
                     ((fn [b] (Long/parseLong b 2))))]
    {:value value
     :length (+ 6 (- (nth (last groups) 2) offset))}))
#_(parse-packet (seq "110100101111111000101000") 0)

(declare parse-packet)
(defn parse-length-operator
  ([bits offset]
   (parse-length-operator bits (+ offset 15) (parse-field bits offset 15) 0 []))
  ([bits offset length parsed-bits packets]
   (if (= length parsed-bits)
     {:length (+ 6 15 1 length)
      :packets packets}
     (let [packet (parse-packet bits offset)]
       (recur bits (+ (:length packet) offset) length (+ parsed-bits (:length packet)) (conj packets packet))))))
#_(parse-packet (seq "00111000000000000110111101000101001010010001001000000000") 0)

(defn parse-count-operator
  [bits offset]
  (let [n (parse-field bits offset 11)]
    (loop [offset (+ offset 11)
           packets []]
      (if (>= (count packets) n)
        {:length (+ 6 11 1 (->> packets (map :length) (reduce +)))
         :packets packets}
        (let [packet (parse-packet bits offset)]
          (recur (+ offset (:length packet))
                 (conj packets packet)))))))
#_(parse-packet (seq "11101110000000001101010000001100100000100011000001100000") 0)

(defn parse-packet
  [bits offset]
  (let [version (parse-field bits offset 3)
        type (parse-field bits (+ offset 3) 3)]
    (->> (cond
           (= type 4) (parse-literal bits (+ 6 offset))
           (zero? (parse-field bits (+ 6 offset) 1)) (parse-length-operator bits (+ offset 7))
           :else (parse-count-operator bits (+ 7 offset)))
         (merge {:version version :type type}))))

(def parsed (parse-packet input 0))

;;16.1
(defn version-sum
  [{:keys [version packets]}]
  (->> packets (map version-sum) (reduce + version)))

(version-sum parsed) ;; => 984

;;16.2
(def ops
  {0 +
   1 *
   2 min
   3 max
   5 #(if (> %1 %2) 1 0)
   6 #(if (< %1 %2) 1 0)
   7 #(if (= %1 %2) 1 0)})

(defn evaluate-packet
  [{:keys [type packets value]}]
  (if (= type 4)
    value
    (->> packets (map evaluate-packet) (reduce (ops type)))))

(evaluate-packet parsed) ;; => 1015320896946
