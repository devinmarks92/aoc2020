(ns aoc2020.day03
  (:require [clojure.string :as s]))

(def input (mapv vec (s/split-lines (slurp "resources/input/day03"))))

(defn trees-encountered
  [x y]
  (let [w (count (first input)) h (count input)]
    (->> {:x 0 :y 0}
         (iterate #(assoc % :x (mod (+ x (:x %)) w) :y (+ y (:y %))))
         (take-while #(<= (:y %) h))
         (filter #(= \# (get-in input [(:y %) (:x %)])))
         count)))

(def part1 (trees-encountered 3 1))

(def part2 (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
                (map #(apply trees-encountered %))
                (apply *)))
