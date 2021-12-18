(ns advent-of-code.2021.day17)

(defn read-input
  [file]
  (let [[min-x max-x min-y max-y] (re-seq #"-?\d+" (slurp file))]
    {:min-x (read-string min-x)
     :max-x (read-string max-x)
     :min-y (read-string min-y)
     :max-y (read-string max-y)}))

(def input (read-input "resources/2021/input17"))

(defn scanner
  [{:keys [min-x max-x min-y max-y]}]
  (for [dx (range (inc max-x))
        dy (range min-y 200) ;dont know what infinite should be brute forcing it for now
        :let [xs (reductions + (concat (range dx 0 -1) (repeat 0)))
              ys (reductions + (range dy (dec min-y) -1))]
        :when (some #(and (<= min-x (first %) max-x)
                          (<= min-y (second %) max-y)) (map vector xs ys))]
    (reduce max ys)))

(->> {:min-x 20 :max-x 30 :min-y -10 :max-y -5} scanner (reduce max)) ;; => 45
(->> input scanner (reduce max)) ;; => 9730
(->> input scanner count) ;; => 4110
