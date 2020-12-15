(ns aoc2020.day14
  (:require [clojure.string :as s]))

(def input (->> (s/split-lines (slurp "resources/input/day14"))
                (map #(remove #{""} (s/split % #"\[|\]|=| ")))
                (map (fn [[instr arg1 arg2]]
                       (if (= instr "mem")
                         [instr (read-string arg1) (read-string arg2)]
                         [instr arg1])))))

(defn apply-mask
  [mask c-set n]
  (->> (s/replace (format "%36s" (Long/toBinaryString n)) " " "0")
       (map #(if (c-set %1) %1 %2) mask)
       (apply str)))

(defn expand-bits
  [v bits]
  (if (s/includes? bits "X")
    (mapcat #(expand-bits v %) (map #(s/replace-first bits "X" %) ["0" "1"]))
    (conj v bits)))

(defn save
  [mem address n mask]
  (assoc mem address (Long/parseLong (apply-mask mask #{\0 \1} n) 2)))

(defn save-v2
  [mem address n mask]
  (let [addresses (expand-bits [] (apply-mask mask #{\1 \X} address))]
    (reduce #(assoc %1 (Long/parseLong %2 2) n) mem addresses)))

(defn execute-instrs
  [save-fn]
  (loop [[[instr arg1 arg2] & instrs] input, mask nil, mem {}]
    (case instr
      "mem" (recur instrs mask (save-fn mem arg1 arg2 mask))
      "mask" (recur instrs arg1 mem)
      nil (apply + (vals mem)))))

(def part1 (execute-instrs save))

(def part2 (execute-instrs save-v2))

