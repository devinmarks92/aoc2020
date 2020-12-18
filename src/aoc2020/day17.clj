(ns aoc2020.day17
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(def input (->> (slurp "resources/test/day17")
                s/split-lines
                (mapv (fn [r] (mapv #(= \# %) r)))))

(defn gen-coord-props [dims]
  (let [i (count input) j (count (first input))]
    {:dims dims
     :bounds (concat [[-1 i] [-1 j]] (repeat (- dims 2) [-1 1]))
     :dirs (->> (apply cartesian-product (repeat dims [-1 0 1]))
                (remove #{(repeat dims 0)}))
     :coords (set (for [x (range i) y (range j) :when (get-in input [x y])]
                    (vec (concat [x y] (repeat (- dims 2) 0)))))}))

(defn get-submatrix
  [dims bounds]
  (apply cartesian-product (map (fn [[i j]] (range i (inc j))) bounds)))

(defn count-neighbors [coords coord dirs]
  (->> (mapv #(mapv + coord %) dirs) (map coords) (remove nil?) count))

(defn activate-coord
  [curr-coords coord {:keys [coords dirs]}]
  (let [n (count-neighbors coords coord dirs) active? (coords coord)]
    (if active?
      (if (>= 3 n 2) curr-coords (disj curr-coords coord))
      (if (= 3 n) (conj curr-coords coord) curr-coords))))

(defn update-coords
  [{:keys [dims dirs bounds coords] :as coord-props}]
  (let [submatrix (get-submatrix dims bounds)
        coords (reduce #(activate-coord %1 %2 coord-props) coords submatrix)]
    (assoc coord-props
           :coords coords
           :bounds (apply mapv (juxt (comp dec min) (comp inc max)) coords))))

(defn simulate [n coord-props]
  (count (:coords (last (take (inc n) (iterate update-coords coord-props))))))

(defn part1 [] (simulate 6 (gen-coord-props 3)))

(defn part2 [] (simulate 6 (gen-coord-props 4)))

