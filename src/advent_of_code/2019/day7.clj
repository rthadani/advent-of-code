(ns advent-of-code.2019.day7
  (:require [advent-of-code.2019.day5 :refer [run-program read-input]]
            [clojure.math.combinatorics :refer [permutations]]))

(defn amplify-signal
  [program phases output]
  (if (empty? phases)
    output
    (let [output (run-program program 0 [(first phases) output] 0 nil)]
      (recur program (rest phases) output))))


(amplify-signal [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4 3 2 1 0] 0)
(amplify-signal [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0 1 2 3 4] 0)

(def program (read-input "resources/2019/input7"))

;;part 1
(->> (permutations [0 1 2 3 4])
     (map #(amplify-signal program % 0))
     (apply max))

;;part 2 what is the final loop i cannot understand how to apply the phases or when to end the feedback loop