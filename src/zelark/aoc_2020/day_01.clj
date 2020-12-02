(ns zelark.aoc-2020.day-01
  (:require [clojure.java.io :as io]))

;; --- Day 1: Report Repair ---
;; https://adventofcode.com/2020/day/1

(def input
  (->> (io/reader (io/resource "input_01.txt"))
       line-seq
       (map #(Long/parseLong %))))

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

(part1 input 2020)
(part2 input 2020)
