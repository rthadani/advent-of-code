(ns advent-of-code.2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input
  [file]
  (with-open [rdr (io/reader file)]
    (->> (line-seq rdr)
         (map (fn [l]  (-> (str/split l #" ")
                           ((fn [[f s]] [f (Integer/parseInt s)])))))
         doall)))

(defn update-pos
  [[h v] [dir cnt]]
  (case dir
    "forward" [(+ h cnt) v]
    "up" [h (- v cnt)]
    "down" [h (+ v cnt)]))

(defn update-with-aim
  [[h v a] [dir cnt]]
  (case dir
    "forward" [(+ h cnt) (+ v (* cnt a)) a]
    "up" [h v (- a cnt)]
    "down" [h v (+ a cnt)]))

;;2.1
(->> (read-input "resources/2021/input2") ;; => 1938402
     (reduce update-pos [0 0])
     (apply *))

;;2.2
(->> (read-input "resources/2021/input2") ;; => 1947878632
     (reduce update-with-aim [0 0 0])
     (take 2)
     (apply *))
