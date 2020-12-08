(ns aoc2020.day5
  (:require [clojure.string :as s]))

(def input (->> (s/split-lines (slurp "resources/input/day5"))
                (map #(-> % (s/replace #"[BR]" "1") (s/replace #"[FL]" "0")))
                (map #(Integer/parseInt % 2))
                sort))

(def part1 (last input))

(def part2 (reduce #(if (= 2 (- %2 %1)) (reduced (inc %1)) %2) input))

