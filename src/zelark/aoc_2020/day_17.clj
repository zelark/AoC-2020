(ns zelark.aoc-2020.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 17: Conway Cubes ---
;; https://adventofcode.com/2020/day/17

(def input (slurp (io/resource "input_17.txt")))

(defn parse-input [input n]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x ch] (when (= ch \#) [x y])) line)) (range))
       (map #(into % (repeat n 0)))
       set))

(defn step [neighbours cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
        loc)))

;; part 1
(defn neighbours-3d [[x y z]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] :when (not= 0 dx dy dz)]
    [(+ x dx) (+ y dy) (+ z dz)]))

(->> (parse-input input 1)
     (iterate (partial step neighbours-3d))
     (drop 6)
     (first)
     (count)) ; 267

;; part 2
(defn neighbours-4d [[x y z w]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] dw [-1 0 1] :when (not= 0 dx dy dz dw)]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(->> (parse-input input 2)
     (iterate (partial step neighbours-4d))
     (drop 6)
     (first)
     (count)) ; 1812
