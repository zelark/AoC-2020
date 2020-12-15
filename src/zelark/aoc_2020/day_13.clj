(ns zelark.aoc-2020.day-13
  (:require [clojure.java.io :as io]))

;; --- Day 13: Shuttle Search ---
;; https://adventofcode.com/2020/day/13

(def input (slurp (io/resource "input_13.txt")))

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (map #(Long/parseLong %))))

(defn parse-input-2 [input]
  (->> (re-seq #"x|\d+" input)
       (rest)
       (keep-indexed #(when-not (= %2 "x") [(Long/parseLong %2) %1]))))

;; part 1
(let [[depart-time & bus-ids] (parse-input input)]
  (->> bus-ids
       (map #(-> [% (- % (mod depart-time %))]))
       (apply min-key second)
       (apply *))) ; 3269

;; part 2
(->> (parse-input-2 input)
     (reduce (fn [[dt acc] [bus-id offset]]
               [(* dt bus-id)
                (->> (iterate (partial + dt) acc)
                     (filter #(zero? (mod (+ % offset) bus-id)))
                     first)]))
     second) ; 672754131923874
