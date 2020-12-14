(ns aoc2020.day13
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "resources/input/day13")))

(def t (read-string (input 0)))

(def buses
  (->> (zipmap (range) (s/split (input 1) #","))
       (reduce-kv #(if (not= "x" %3) (assoc %1 %2 (read-string %3)) %1) {})))

(def part1
  (apply * (apply min-key second (map (fn [[_ v]] [v (- v (mod t v))]) buses))))

(def part2
  (loop [[[idx bus] & buses] buses, i 1, c 0]
    (if (nil? bus)
      c
      (let [t (mod idx bus)
            cs (iterate #(+ i %) c)
            c (first (filter #(= (mod % bus) (if (> t 0) (- bus t) 0)) cs))]
        (recur buses (* i bus) c)))))

