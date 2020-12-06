(ns zelark.aoc-2020.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 6: Custom Customs ---
;; https://adventofcode.com/2020/day/6

(def input (slurp (io/resource "input_06.txt")))

(defn parse-input [input]
  (->> (str/split input #"\R\R")
       (map str/split-lines)
       (map (partial map set))))

;; part 1
(->> (parse-input input)
     (map (partial apply set/union))
     (map count)
     (apply +)) ; 6735

;; part 2
(->> (parse-input input)
     (map (partial apply set/intersection))
     (map count)
     (apply +)) ; 3221
