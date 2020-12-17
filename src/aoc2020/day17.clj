(ns aoc2020.day17
  (:require [clojure.string :as s]))

(def input (->> (slurp "resources/test/day17")
                s/split-lines
                (mapv (fn [r] (mapv #(= \# %) r)))))

;; generate n-dimensional coords: (vec (concat [x y] (repeat (- n 2) 0)))
(def coord-props
  {:bounds [[-1 (count input)] [-1 (count (first input))] [-1 1] [-1 1]]
   :coords (into #{} (for [x (range (count input))
                           y (range (count (first input)))
                           :when (get-in input [x y])]
                       [x y 0 0]))})

(def dirs (remove #{[0 0 0 0]} (for [x [-1 0 1] y [-1 0 1] z [-1 0 1] w [-1 0 1]] [x y z w])))

(defn get-submatrix
  [[[x-floor x-ceil] [y-floor y-ceil] [z-floor z-ceil] [w-floor w-ceil]]]
  (for [x (range x-floor (inc x-ceil))
        y (range y-floor (inc y-ceil))
        z (range z-floor (inc z-ceil))
        w (range w-floor (inc w-ceil))]
    [x y z w]))

(defn count-neighbors [coords coord]
  (->> (mapv #(mapv + coord %) dirs) (map coords) (remove nil?) count))

(defn activate-coord [coords coord prev]
  (let [n (count-neighbors prev coord), active? (prev coord)]
    (if active?
      (if (>= 3 n 2) coords (disj coords coord))
      (if (= 3 n) (conj coords coord) coords))))

(defn update-coords
  [{:keys [bounds coords] :as coord-props}]
  (let [submatrix (get-submatrix bounds)
        coords (reduce #(activate-coord %1 %2 coords) coords submatrix)]
    {:coords coords
     :bounds (apply mapv (juxt (comp dec min) (comp inc max)) coords)}))

(defn part1 []
  (count (:coords (last (take 7 (iterate update-coords coord-props))))))

