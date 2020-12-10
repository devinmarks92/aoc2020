(ns aoc2020.day02
  (:require [clojure.string :as s]))

(defn format-input
  [line]
  (-> (s/split line #"-| |: ")
      (update 0 read-string)
      (update 1 read-string)
      (update 2 first)))

(def input (map format-input (s/split-lines (slurp "resources/input/day02"))))

(defn valid-freq-password?
  [[min max c password]]
  (let [n ((frequencies password) c 0)]
    (<= min n max)))

(defn valid-index-password?
  [[i j c password]]
  (let [x (get password (dec i)), y (get password (dec j))]
    (and (not= x y) (or (= c x) (= c y)))))

(def part1 (count (filter valid-freq-password? input)))

(def part2 (count (filter valid-index-password? input)))

