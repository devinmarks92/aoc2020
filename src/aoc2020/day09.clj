(ns aoc2020.day09
  (:require [clojure.string :as s]))

(def input (mapv read-string (s/split-lines (slurp "resources/input/day09"))))

(defn two-sum
  [v n]
  (let [v-set (set v)]
    (first (for [x v :let [y (v-set (- n x))] :when y] true))))

(def part1
  (loop [i 0 j 25]
    (let [subv (subvec input i j), n (input j)]
      (if (two-sum subv n) (recur (inc i) (inc j)) n))))

(def part2
  (let [v (vec (take-while #(not= part1 %) input))]
    (loop [i 0 j 1]
      (let [subv (subvec v i j), sum (apply + subv)]
        (cond (= part1 sum) (apply + (apply (juxt min max) subv))
              (> part1 sum) (recur i (inc j))
              (< part1 sum) (recur (inc i) j))))))

