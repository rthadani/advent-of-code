(ns advent-of-code.2018.day13
  (:require [clojure.string :as str]))

(def track-symbols
  {\-  \-
   \<  \-
   \>  \- 
   \|  \| 
   \^ \|
   \v \|
   \/ \/
   \\ \\
   \+ \+})

(defn build-track
  [input]
  (let [width (apply max (map #(count %) input))
        height (count input)]
    (into {}
          (for [y (range 0 height)
                x (range 0 width)
                :let [c (nth x (nth y input))]
                :when (not= (str c) " ")]
            [[x y] (track-symbols c)]))))

(defn build-carts
  [input]
(let [width (apply max (map #(count %) input))
      height (count input)]
  (for [y (range 0 height)
        x (range 0 width)
        :let [c (nth x (nth y input))]
        :when (or (= (str c) "<")
                  (= (str c) ">")
                  (= (str c) "^")
                  (= (str c) "v"))]
    {:id [x y] :state c :pos [x y] :next-i :l})))

(defn intersection-cycle
  [i dir]
  (case i
    :l :s
    :s :r
    :r :l))

(defn dir-at-intersection
  [{:keys [next-i state] :as cart}]
  (case next-i
    :s \-
    :l (if (#{\v \<} #{state}) \/ \\)
    :r (if (#{\^ \>} #{state}) \/ \\)))

(defn next-track-coord
  [{:keys [state pos]}]
   (case state
     \< [(dec (first pos)) (second pos)]  
     \> [(inc (first pos)) (second pos)]
     \^ [(first pos) (dec (second pos))]
     \v [(first pos) (inc (second pos))]))

(defn make-turn-or-move-to-next
  [{:keys [state] :as cart} dir next-cord]
  (condp = (str state dir)
    ">\\" (assoc cart :state \v)
    "<\\" (assoc cart :state \^)
    "^\\" (assoc cart :state \<)
    "v\\" (assoc cart :state \>)
    ">/" (assoc cart :state \^)
    "</" (assoc cart :state \v)
    "^/" (assoc cart :state \>)
    "v/" (assoc cart :state \<)
    :else (assoc cart :pos next-cord)))

(defn move-cart
  [{:keys [state] :as cart} tracks]
  (let [next-maybe-pos (next-track-coord cart)
      next-track (get tracks next-maybe-pos)]
    (condp = (str state next-track)
      ">+" (make-turn-or-move-to-next cart (dir-at-intersection cart) next-maybe-pos)
      "<+" (make-turn-or-move-to-next cart (dir-at-intersection cart) next-maybe-pos)
      "^+" (make-turn-or-move-to-next cart (dir-at-intersection cart) next-maybe-pos)
      "v+" (make-turn-or-move-to-next cart (dir-at-intersection cart) next-maybe-pos)
      :else (make-turn-or-move-to-next cart next-track next-maybe-pos))))

(defn crash-location
  [carts]
  (->> (group-by :pos carts)
       (filter (fn [[_ v] (> 1 (count v))]))
       first)

(defn move-carts [tracks carts]
  (for [cart carts]
    (move-cart tracks)))

(defn part1(input)
  (let [ track (build-track input)
        carts (build-carts input)])
  (->> (iterate (partial move-carts tracks) carts)
     (take-while #(not (crash-location %))) 
     last)