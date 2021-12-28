(ns advent-of-code.2021.day25
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input
  [file]
  (let [lines (-> file slurp str/split-lines)
        coords (for [h (range 0 (count lines))
                     w (range 0 (count (lines 0)))]
                 (condp = (nth (lines h) w)
                   \. [nil [h w]]
                   \> [:east [h w]]
                   \v [:south [h w]]))]
    {:move :east
     :width (count (lines 0))
     :height (count lines)
     :cucumbers (->> coords
                     (remove (comp nil? first))
                     (group-by first)
                     (map (fn [[k vs]] [k (set (map second vs))]))
                     (into {}))}))

(def input (read-input "resources/2021/input25"))

(defn next-move
  [{:keys [move width height]} cucumber-group-positions [x y]]
  (let [new-coords (if (= move :south) [(mod (inc x) height) y] [x (mod (inc y) width)])]
    (if (cucumber-group-positions new-coords)
      [x y]
      new-coords)))

(defn half-step
  [{:keys [move cucumbers] :as input}]
  (let [to-update-coords (get cucumbers move)
        all-cucumbers (set/union (:east cucumbers) (:south cucumbers))
        future-move (if (= move :south) :east :south)]
    (->>
     {:cucumbers (->> {move (set (map #(next-move input  all-cucumbers %) to-update-coords))} (merge cucumbers))}
     (merge input {:move future-move}))))

(defn step
  [input]
  (half-step (half-step input)))

(defn moving?
  [previous current]
  (not (and (= (get-in previous [:cucumbers :south]) (get-in current [:cucumbers :south]))
            (= (get-in previous [:cucumbers :east]) (get-in current [:cucumbers :east])))))

(->> (iterate step input) ;; => 435
     (partition 2 1)
     (take-while #(apply moving? %))
     (count)
     inc)
