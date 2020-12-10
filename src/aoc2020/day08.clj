(ns aoc2020.day08
  (:require [clojure.string :as s]))

(def input (mapv #(update (s/split % #" ") 1 read-string)
                 (s/split-lines (slurp "resources/input/day08"))))

(defn execute-instrs
  [input terminate?]
  (loop [acc 0, i 0, idxs #{}]
    (let [[instr arg] (get input i)]
      (cond (idxs i) (if terminate? nil acc)
            (nil? instr) acc
            (= instr "nop") (recur acc (inc i) (conj idxs i))
            (= instr "jmp") (recur acc (+ i arg) (conj idxs i))
            (= instr "acc") (recur (+ acc arg) (inc i) (conj idxs i))))))

(def part1 (execute-instrs input false))

(def part2
  (let [r {"nop" "jmp", "jmp" "nop"}]
    (->> (range (count input))
         (filter #((set (keys r)) (get-in input [% 0])))
         (map #(update input % (fn [[instr v]] [(r instr) v])))
         (some #(execute-instrs % true)))))
