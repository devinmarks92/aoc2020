(ns aoc2020.day11
  (:require [clojure.string :as s]))

(def input (mapv vec (s/split-lines (slurp "resources/input/day11"))))

(def directions [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]])

(defn adjacent-neighbors
  [v coord]
  (count (filter #(= \# (get-in v %)) (map (partial map + coord) directions))))

(defn visible-neighbor
  [v coord direction]
  (loop [next-coord (map + direction coord)]
    (let [c (get-in v next-coord)]
      (if (= \. c) (recur (map + direction next-coord)) c))))

(defn visible-neighbors
  [v coord]
  (count (filter #(= \# %) (map #(visible-neighbor v coord %) directions))))

(defn get-next
  [v coord n neighbors-fn]
  (case (get-in v coord)
    \. \.
    \L (if (zero? (neighbors-fn v coord)) \# \L)
    \# (if (<= n (neighbors-fn v coord)) \L \#)))

(defn step
  [v n neighbors-fn]
  (let [coords (for [x (range (count input)) y (range (count (input 0)))] [x y])]
    (reduce #(assoc-in %1 %2 (get-next v %2 n neighbors-fn)) v coords)))

(defn stabilize
  [n neighbors-fn]
  (loop [v input]
    (let [v' (step v n neighbors-fn)]
      (if (= v v')
        (count (mapcat #(filter (partial = \#) %) v'))
        (recur v')))))

(def part1 (stabilize 4 adjacent-neighbors))

(def part2 (stabilize 5 visible-neighbors))
