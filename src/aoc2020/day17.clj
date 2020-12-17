(ns aoc2020.day17
  (:require [clojure.string :as s]))

(def input (->> (slurp "resources/input/day17")
                s/split-lines
                (mapv (fn [r] (mapv #(= \# %) r)))))

(def coords (into #{} (for [x (range (count input))
                            y (range (count (first input)))
                            :when (get-in input [x y])]
                        [x y 0])))

(def coord-props {:x-bound [-1 (count input)]
                  :y-bound [-1 (count (first input))]
                  :z-bound [-1 1]
                  :coords coords})

(def dirs (remove #{[0 0 0]} (for [x [-1 0 1] y [-1 0 1] z [-1 0 1]] [x y z])))

(defn get-bound-coords
  [[x-floor x-ceil] [y-floor y-ceil] [z-floor z-ceil]]
  (for [x (range x-floor (inc x-ceil))
        y (range y-floor (inc y-ceil))
        z (range z-floor (inc z-ceil))]
    [x y z]))

(defn count-neighbors [coords coord]
  (->> (mapv #(mapv + coord %) dirs) (map coords) (remove nil?) count))

(defn activate-coord
  [coords coord prev]
  (let [n (count-neighbors prev coord), active? (prev coord)]
    (if active?
      (if (>= 3 n 2) coords (disj coords coord))
      (if (= 3 n) (conj coords coord) coords))))

(defn update-coords
  [{:keys [x-bound y-bound z-bound coords] :as coord-props}]
  (let [bound-coords (get-bound-coords x-bound y-bound z-bound)]
    {:coords (reduce #(activate-coord %1 %2 coords) coords bound-coords)
     :x-bound [(dec (x-bound 0)) (inc (x-bound 1))]
     :y-bound [(dec (y-bound 0)) (inc (y-bound 1))]
     :z-bound [(dec (z-bound 0)) (inc (z-bound 1))]}))

(defn part1 []
  (count (:coords (last (take 7 (iterate update-coords coord-props))))))

