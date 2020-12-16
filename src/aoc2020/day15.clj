(ns aoc2020.day15
  (:require [clojure.string :as s]))

(def input (map read-string (s/split (slurp "resources/input/day15") #",")))

(defn recitation
  [m]
  (loop [i (count input) n (last input) prev-n (zipmap input (map inc (range)))]
    (if (< i m)
      (recur (inc i) (if (prev-n n) (- i (prev-n n)) 0) (assoc prev-n n i))
      n)))

(defn part1 [] (recitation 2020))

(defn part2 [] (recitation 30000000))
