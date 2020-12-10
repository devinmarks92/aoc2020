(ns aoc2020.day07
  (:require [clojure.string :as s]))

(defn format-input-line
  [m line]
  (assoc m
         ((re-find #"(.+?) bags contain" line) 1)
         (reduce #(assoc %1 (%2 2) (read-string (%2 1)))
                 {}
                 (re-seq #"(\d+) (.+?) bags?[,.]" line))))

(def input (->> (slurp "resources/input/day07")
                s/split-lines
                (reduce format-input-line {})))

(defn contains-bag?
  [bag target]
  (or (= bag target) (some #(contains-bag? (key %) target) (input bag))))

(defn count-nested-bags
  [bag]
  (apply + (map (fn [[k v]] (+ v (* v (count-nested-bags k)))) (input bag))))

(def part1 (dec (count (filter #(contains-bag? % "shiny gold") (keys input)))))

(def part2 (count-nested-bags "shiny gold"))
