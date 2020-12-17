(ns aoc2020.day16
  (:require [clojure.string :as s]
            [clojure.set :as sets]))

(def input (map #(s/split % #"\n")
                (s/split (slurp "resources/input/day16") #"\n\n")))

(def input-values
  (reduce (fn [m [k v1 v2 v3 v4]]
            (let [r1 (range (read-string v1) (inc (read-string v2)))
                  r2 (range (read-string v3) (inc (read-string v4)))]
              (assoc m k (set (concat r1 r2)))))
          {}
          (map #(s/split % #": |-| or |,") (first input))))

(def ticket (mapv read-string (s/split (second (second input)) #",")))

(def nearby-tickets
  (map #(map read-string (s/split % #",")) (rest (last input))))

(defn remove-invalid
  [ticket]
  (remove (apply sets/union (vals input-values)) ticket))

(defn part1 [] (apply + (remove-invalid (flatten nearby-tickets))))

(def valid-tickets (filter #(empty? (remove-invalid %)) nearby-tickets))

(defn match-rows
  [rows]
  (loop [rows rows, valid {}]
    (if-let [row (first (keep-indexed (fn [i r] (if (= 1 (count r)) i)) rows))]
      (let [name (first (rows row))]
        (recur (mapv #(disj % name) rows) (assoc valid row name)))
      (reduce #(assoc %1 (valid %2) (ticket %2)) {} (range (count ticket))))))

(defn part2
  []
  (->> (apply map (comp set vector) (conj valid-tickets ticket))
       (map #(map (fn [[k v]] (if (sets/subset? % v) k)) input-values))
       (mapv #(set (remove nil? %)))
       match-rows
       (filter (fn [[k v]] (s/includes? k "departure")))
       (map second)
       (apply *)))

