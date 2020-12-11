(ns aoc2020.day11
  (:require [clojure.string :as s]))

(def input (mapv vec (s/split-lines (slurp "resources/input/day11"))))

(def directions [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]])

(defn adjacent-neighbors
  [v coord]
  (count (filter #(= \# (get-in v %)) (map (partial map + coord) directions))))

(defn visible-neighbor
  [v coord direction]
  (->> (drop 1 (iterate (partial map + direction) coord))
       (map #(get-in v %))
       (filter #(not= \. %))
       first))

(defn visible-neighbors
  [v coord]
  (count (filter #(= \# %) (map #(visible-neighbor v coord %) directions))))

(defn get-next
  [v coord neighbors neighbors-fn]
  (case (get-in v coord)
    \. \.
    \L (if (zero? (neighbors-fn v coord)) \# \L)
    \# (if (<= neighbors (neighbors-fn v coord)) \L \#)))

(defn step
  [v neighbors neighbors-fn]
  (let [coords (for [x (range (count input)) y (range (count (input 0)))] [x y])]
    (reduce #(assoc-in %1 %2 (get-next v %2 neighbors neighbors-fn)) v coords)))

(defn stabilize
  [neighbors neighbors-fn]
  (loop [v input]
    (let [v' (step v neighbors neighbors-fn)]
      (if (= v v')
        (count (mapcat #(filter (partial = \#) %) v'))
        (recur v')))))

(def part1 (stabilize 4 adjacent-neighbors))

(def part2 (stabilize 5 visible-neighbors))
