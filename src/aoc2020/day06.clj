(ns aoc2020.day6
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(def input (map #(map set (s/split % #"\n"))
                (s/split (slurp "resources/input/day6") #"\n\n")))

(def part1 (apply + (map #(count (apply sets/union %)) input)))

(def part2 (apply + (map #(count (apply sets/intersection %)) input)))

