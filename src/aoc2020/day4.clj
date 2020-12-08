(ns aoc2020.day4
  (:require [clojure.string :as s]))

(defn format-input
  [line]
  (into {} (map #(s/split % #":") (s/split line #"[ \n]"))))

(def input (map format-input (s/split (slurp "resources/input/day4") #"\n\n")))

(defn required-fields?
  [passport]
  (every? passport ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))

(defn valid-passport?
  [{:strs [byr iyr eyr hcl ecl pid hgt] :as passport}]
  (and (required-fields? passport)
       (<= 1920 (read-string byr) 2002)
       (<= 2010 (read-string iyr) 2020)
       (<= 2020 (read-string eyr) 2030)
       (re-find #"#[a-f0-9]{6}" hcl)
       (some #(= ecl %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
       (= 9 (count pid))
       (cond (s/ends-with? hgt "cm")
             (<= 150 (read-string (s/replace hgt #"cm" "")) 193)
             (s/ends-with? hgt "in")
             (<= 59 (read-string (s/replace hgt #"in" "")) 76))))

(def part1 (count (filter required-fields? input)))

(def part2 (count (filter valid-passport? input)))

