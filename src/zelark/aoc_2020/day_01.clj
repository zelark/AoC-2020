(ns zelark.aoc-2020.day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 1: Report Repair ---
;; https://adventofcode.com/2020/day/1

(def input (slurp (io/resource "input_01.txt")))

(defn parse-input [input]
  (->> input (str/split-lines) (map #(Long/parseLong %))))

(defn part1 [numbers target]
  (first
   (for [[a & a-rest] (iterate next numbers) :while a
         b a-rest :when (== (+ a b) target)]
     (* a b))))

(defn part2 [numbers target]
  (first
   (for [[a & a-rest] (iterate next numbers) :while a
         [b & b-rest] (iterate next a-rest)  :while b
         c            b-rest                 :when (== (+ a b c) target)]
     (* a b c))))

(part1 (parse-input input) 2020) ; 440979
(part2 (parse-input input) 2020) ; 82498112
