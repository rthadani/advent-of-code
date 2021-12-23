(ns advent-of-code.2021.day22
  (:require
   [clojure.string :as str]
   [clojure.core.reducers :as r]))

(defn parse-line
  [l]
  (let  [range-regex "((-?\\d+)\\.\\.(-?\\d+))"
         regex (re-pattern (apply format "(on|off) x=%s,y=%s,z=%s" (repeat 3 range-regex)))
         x (re-find regex l)
         [_ action _ sx ex _ sy ey _ sz ez] (re-find regex l)]
    {:action (keyword action)
     :x [(read-string sx) (read-string ex)]
     :y [(read-string sy) (read-string ey)]
     :z [(read-string sz) (read-string ez)]}))

(defn read-input
  [file]
  (->> file slurp str/split-lines (map parse-line)))

(def c1 (parse-line "on x=-1..48,y=-16..38,z=-20..25"))
(->> (merge-with (fn [v1 v2] (if (vector? v1) (vec (concat v1 v2)) v1)) c1 c1))

(defn overlap? [c1 c2]
  (and (<= (get-in c2 [:x 0]) (get-in c1 [:x 1])) (<= (get-in c1 [:x 0]) (get-in c2 [:x 1]))
       (<= (get-in c2 [:y 0]) (get-in c1 [:y 1])) (<= (get-in c1 [:y 0]) (get-in c2 [:y 1]))
       (<= (get-in c2 [:z 0]) (get-in c1 [:z 1])) (<= (get-in c1 [:z 0]) (get-in c2 [:z 1]))))

#_(overlap? {:action :on, :x [10 12], :y [10 12], :z [10 12]} {:action :on, :x [11 13], :y [11 13], :z [11 13]})

(defn split-cuboid
  [c1 c2]
  (reduce (fn [cuboids [coord [c1min c1max c2min c2max]]]
            (let [x-mid [(max (get-in c1 [:x 0]) (get-in c2 [:x 0])) (min (get-in c1 [:x 1]) (get-in c2 [:x 1]))]
                  y-mid [(max (get-in c1 [:y 0]) (get-in c2 [:y 0])) (min (get-in c1 [:y 1]) (get-in c2 [:y 1]))]]
              (cond
                (= coord :x) (cond-> cuboids
                               (> c2min c1min) (conj {:x [c1min (dec c2min)] :y (:y c1) :z (:z c1)})
                               (< c2max c1max) (conj {:x [(inc c2max) c1max] :y (:y c1) :z (:z c1)}))
                (= coord :y) (cond-> cuboids
                               (> c2min c1min) (conj {:x x-mid :y [c1min (dec c2min)] :z (:z c1)})
                               (< c2max c1max) (conj {:x x-mid :y [(inc c2max) c1max] :z (:z c1)}))
                :else (cond-> cuboids
                        (> c2min c1min) (conj {:x x-mid :y y-mid :z [c1min (dec c2min)]})
                        (< c2max c1max) (conj {:x x-mid :y y-mid :z [(inc c2max) c1max]})))))
          []
          (-> (merge-with (fn [v1 v2] (when (vector? v1) (vec (concat v1 v2)))) c1 c2) (select-keys [:x :y :z]))))

#_(split-cuboid {:action :on, :x [10 12], :y [10 12], :z [10 12]} {:action :on, :x [11 13], :y [11 13], :z [11 13]})

(defn volume [{:keys [x y z]}]
  (* (inc (- (x 1) (x 0)))
     (inc (- (y 1) (y 0)))
     (inc (- (z 1) (z 0)))))

(defn execute-instructions
  [instructions]
  (loop [[i & r] instructions
         lit []]
    (if (nil? i)
      lit
      (recur r
             (r/reduce (fn [lit l]
                         (if (overlap? l i)
                           (concat lit (split-cuboid l i)) ;;keep only part of the lit cubes that are non overlapping
                           (conj lit l)))
                       (if (= (:action i) :on) [i] []) lit)))))

#_(->> (execute-instructions [{:action :on, :x [10 12], :y [10 12], :z [10 12]}
                              {:action :on, :x [11 13], :y [11 13], :z [11 13]}])
       (map volume)
       (apply +))

(def input (read-input "resources/2021/input22"))

;;22.1
(->> input ;; => 588120
     (filter #(every? (fn [v] (<= (Math/abs v) 50)) (apply concat (vals (select-keys % [:x :y :z])))))
     (execute-instructions)
     (map volume)
     (apply +))

;;22.2
(time (->> input  ;; => 1134088247046731
           (execute-instructions)
           (map volume)
           (apply +)))
