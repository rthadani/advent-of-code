(ns advent-of-code.2019.day9)


(def input (slurp "resources/2019/input8"))

(defn input->layers [input width height]
  (->> (partition (* width height) input)
       #_(map #(partition width %))))

(input->layers "123456789012" 3 2)

(defn layer-with-fewest-zeroes
 [layers]
 (->> layers
      (map #(frequencies %))
      (apply min-key (fn [f] (get f \0 0)))))


(layer-with-fewest-zeroes (input->layers "123456789012" 3 2))

;;part 1
(apply * (-> (input->layers input 25 6)
             layer-with-fewest-zeroes
             (select-keys [\1 \2])
             vals)) 

;;part 2
(def layers (input->layers input 25 6))
 (->> layers
      (apply interleave)
      (partition (count layers))
      (map (fn [pixels] (first (filter #(not= % \2) pixels))))
      (map #(if (= % \1) "#" " "))
      (partition 25))
