(ns zelark.aoc-2020.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 15: Rambunctious Recitation ---
;; https://adventofcode.com/2020/day/15

(def input (slurp (io/resource "input_15.txt")))

(defn parse-input [input]
  (let [numbers (->> (str/split input #"[,\n]")
                     (map #(Long/parseLong %)))]
    (zipmap numbers (range 1 (inc (count numbers))))))

(defn solve [input limit]
  (loop [spoken (parse-input input)
         n      (inc (count spoken))
         number 0]
    (if (== n limit)
      number
      (if-let [last-time (spoken number)]
        (recur (assoc spoken number n) (inc n) (- n last-time))
        (recur (assoc spoken number n) (inc n) 0)))))

;; part 1
(solve input 2020) ; 232

;; part 2
(solve input 30000000) ; 18929178
