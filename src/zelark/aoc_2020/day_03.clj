(ns zelark.aoc-2020.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 3: Toboggan Trajectory ---
;; https://adventofcode.com/2020/day/3

(def input (slurp (io/resource "input_03.txt")))

(defn tree? [row x]
  (= (nth row (mod x (count row))) \#))

(defn check-slope [input [right down]]
  (first
   (reduce (fn [[n x] row]
             [(cond-> n (tree? row x) inc)
              (+ x right)])
           [0 right]
           (rest (take-nth down input)))))

;; part 1
(check-slope (str/split-lines input) [3 1]) ; 274

;; part 2
(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(->> (map (partial check-slope (str/split-lines input)) slopes)
     (apply *)) ; 6050183040
