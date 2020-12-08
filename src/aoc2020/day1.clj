(ns aoc2020.day1
  (:require [clojure.string :as s]))

(def input (map read-string (s/split-lines (slurp "resources/input/day1"))))

(def input-set (set input))

(def part1
  (first (for [x input
               :let [e (input-set (- 2020 x))]
               :when e]
           (* x e))))

(def part2
  (first (for [x input
               y (rest input)
               :let [e (input-set (- 2020 (+ x y)))]
               :when e]
           (* x y e))))

