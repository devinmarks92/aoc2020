(ns aoc2020.day12
  (:require [clojure.string :as s]))

(def input (->> (s/split-lines (slurp "resources/test/day12"))
                (map #(split-at 1 %))
                (mapv (fn [[v1 v2]] [(first v1) (read-string (apply str v2))]))))

(def dir-map {\N [0 1] \S [0 -1] \E [1 0] \W [-1 0]})

(def rotate-map {\R {\N \E, \E \S, \S \W, \W \N}
                 \L {\N \W, \W \S, \S \E, \E \N}})

(defn move
  [dir loc n]
  (reduce (partial mapv +) loc (repeat n dir)))

(defn rotate
  [action dir n]
  (last (take (inc (/ n 90)) (iterate (rotate-map action) dir))))

(defn rotate-waypoint
  [action [x y] n]
  (let [n (if (= action \L) n (* n -1)), radians (Math/toRadians n)]
    [(Math/round (- (* x (Math/cos radians)) (* y (Math/sin radians))))
     (Math/round (+ (* y (Math/cos radians)) (* x (Math/sin radians))))]))

(def part1
  (loop [dir \E loc [0 0] [[action n] & instrs] input]
    (condp = action
      nil (apply + (map #(Math/abs %) loc))
      \F (recur dir (move (dir-map dir) loc n) instrs)
      \L (recur (rotate action dir n) loc instrs)
      \R (recur (rotate action dir n) loc instrs)
      (recur dir (move (dir-map action) loc n) instrs))))

(def part2
  (loop [waypoint [10 1] loc [0 0] [[action n] & instrs] input]
    (condp = action
      nil (apply + (map #(Math/abs %) loc))
      \F (recur waypoint (move waypoint loc n) instrs)
      \L (recur (rotate-waypoint action waypoint n) loc instrs)
      \R (recur (rotate-waypoint action waypoint n) loc instrs)
      (recur (move (dir-map action) waypoint n) loc instrs))))
