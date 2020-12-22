(ns zelark.aoc-2020.day-12b
  (:require [clojure.java.io :as io]))

;; --- Day 12: Rain Risk ---
;; https://adventofcode.com/2020/day/12

(def input (slurp (io/resource "input_12.txt")))

(defn parse-input [input]
  (->> (re-seq #"([NSEWLRF])(\d+)" input)
       (map (fn [[_ action value]] [(keyword action) (Long/parseLong value)]))))

;; https://en.wikipedia.org/wiki/Rotation_matrix#Common_rotations
(defn rotate [{:keys [dx dy] :as ship} value]
  (let [[a b c d] ({90 [0 1 -1 0], 180 [-1 0 0 -1], 270 [0 -1 1 0]} value)]
    (assoc ship
           :dx (+ (* dx a) (* dy b))
           :dy (+ (* dx c) (* dy d)))))

(defn move [[kx ky]
            {:keys [x y dx dy] :as ship}
            [cmd val]]
  (case cmd
    :F (assoc ship :x (+ x (* dx val)) :y (+ y (* dy val)))
    :E (update ship kx + val)
    :W (update ship kx - val)
    :N (update ship ky + val)
    :S (update ship ky - val)
    :L (rotate ship (- 360 val))
    :R (rotate ship val)))

(defn distance [step [dx dy] input]
  (->> (parse-input input)
       (reduce step {:x 0 :y 0 :dx dx :dy dy})
       ((fn [{x :x y :y}] (+ (Math/abs x) (Math/abs y))))))

(distance (partial move [:x :y]) [1 0] input)
(distance (partial move [:dx :dy]) [10 1] input)
