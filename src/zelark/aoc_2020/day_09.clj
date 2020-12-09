(ns zelark.aoc-2020.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 9: Encoding Error ---
;; https://adventofcode.com/2020/day/9

(def input (slurp (io/resource "input_09.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(Long/parseLong %))))

(defn combinations [coll]
  (for [[x & x-rest] (iterate next coll) :while x
        y            x-rest]
    [x y]))

(defn valid? [window num]
  (boolean (some #(== num (apply + %)) (combinations window))))

(defn part1 [numbers size]
  (some identity
        (map (fn [window num] (when-not (valid? window num) num))
             (partition size 1 numbers)
             (drop size numbers))))

(part1 (parse-input input) 25) ; 41682220

(defn part2 [numbers target]
  (some identity
        (for [xs (iterate next numbers) :while xs]
          (reduce (fn [acc y]
                    (let [sum (+ acc y)
                          rng (take-while (partial not= y) xs)]
                      (cond
                        (== target sum) (reduced (+ (apply min rng) (apply max rng)))
                        (<  target sum) (reduced nil)
                        :else           sum)))
                  xs))))

(part2 (parse-input input) 41682220) ; 5388976
