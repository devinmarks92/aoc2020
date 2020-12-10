(ns aoc2020.day10
  (:require [clojure.string :as s]))

(def input
  (let [ints (map read-string (s/split-lines (slurp "resources/input/day10")))]
    (vec (concat [0] (sort ints) [(+ 3 (apply max ints))]))))

(def part1
  (apply * (vals (frequencies (map #(apply - %) (partition 2 1 input))))))

(defn count-arrangements-at-i
  [d i]
  (reduce (fn [n j] (if (>= 3 (- (input i) (input j))) (+ n (d j)) n))
          0
          (reverse (range (max 0 (- i 3)) i))))

(def part2
  (last (reduce (fn [d i] (conj d (count-arrangements-at-i d i)))
                [1]
                (range 1 (count input)))))
