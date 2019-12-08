(ns advent-ofcode.2019.day5
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn read-input [file]
  (as-> (slurp file) $
    (str/split $ #",")
    (s/transform [s/ALL] #(Integer/parseInt %) $)))

(defn replace-value
  [index value arr]
  (assoc arr index value))

(defn get-value
  [param operation program]
  (case param
    :c (if (= 0 (get operation param)) (nth program (nth program (+ (get operation :index) 1))) (nth program (+ (get operation :index) 1)))
    :b (if (= 0 (get operation param)) (nth program (nth program (+ (get operation :index) 2))) (nth program (+ (get operation :index) 2)))
    :a (if (= 0 (get operation param)) (nth program (nth program (+ (get operation :index) 3))) (nth program (+ (get operation :index) 3)))))

(defn code-add
  [program operation]
  (replace-value (nth program (+ 3 (get operation :index))) (+ (get-value :c operation program) (get-value :b operation program)) program))

(defn code-mul
  [program operation]
  (replace-value (nth program (+ 3 (get operation :index))) (* (get-value :c operation program) (get-value :b operation program)) program))

(defn code-input
  [program operation input]
  (replace-value (nth program (+ 1 (get operation :index))) input program))

(defn code-output
  [program operation]
  (do
    (println (get-value :c operation program))
    program))

(defn code-jump-false
  [operation program]
  (if (= 0 (get-value :c operation program)) (get-value :b operation program) (+ 3 (get operation :index))))

(defn code-jump-true
  [operation program]
  (if (not= 0 (get-value :c operation program)) (get-value :b operation program) (+ 3 (get operation :index))))

(defn code-less
  [operation program]
  (let [value (if (< (get-value :c operation program) (get-value :b operation program)) 1 0)]
    (replace-value (nth program (+ 3 (get operation :index))) value program)))

(defn code-equal
  [operation program]
  (let [value (if (= (get-value :c operation program) (get-value :b operation program)) 1 0)]
    (replace-value (nth program (+ 3 (get operation :index))) value program)))

(defn digits
  [number]
  (map #(Character/digit % 10) (format "%05d" number)))

(defn get-op-code
  [digits]
  (Integer/parseInt (str/join (take-last 2 digits))))

(defn get-param-mode
  [digits index]
  (Integer/parseInt (str (nth digits index \0))))

(defn get-operation
  [program index]
  (let [digits (digits (nth program index))]
    {:de (get-op-code digits) :c (get-param-mode digits 2) :b (get-param-mode digits 1) :a (get-param-mode digits 0) :index index}))

(defn apply-code
  [program index input]
  (let [operation (get-operation program index)]
    (case (get operation :de)
      1 {:step (+ 4 index) :program (code-add program operation)}
      2 {:step (+ 4 index) :program (code-mul program operation)}
      3 {:step (+ 2 index) :program (code-input program operation input)}
      4 {:step (+ 2 index) :program (code-output program operation)}
      5 {:step (code-jump-true operation program) :program program}
      6 {:step (code-jump-false operation program) :program program}
      7 {:step (+ 4 index) :program (code-less operation program)}
      8 {:step (+ 4 index) :program (code-equal operation program)}
      99 :done)))

(defn run-program
  [program pointer input]
  (let [result (apply-code program pointer input)]
    (if (= result :done)
      program
      (recur (get result :program) (get result :step) input))))

(def part1 (run-program (read-input "resources/2019/input5") 0 1))

(def part2 (run-program (read-input "resources/2019/input5") 0 5))
