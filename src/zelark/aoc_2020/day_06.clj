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

(defn solve [join-fn input]
  (->> (parse-input input)
       (map (partial apply join-fn))
       (map count)
       (apply +)))

;; part 1
(solve set/union input) ; 6735

;; part 2
(solve set/intersection input) ; 3221
