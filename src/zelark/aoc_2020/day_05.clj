(ns zelark.aoc-2020.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 5: Binary Boarding ---
;; https://adventofcode.com/2020/day/5

(def input (str/split-lines (slurp (io/resource "input_05.txt"))))

(defn- decode [code]
  (let [b (->> code (map {\F 0 \B 1 \R 1 \L 0}) (apply str))]
    (Long/parseLong b 2)))

(defn decode-seat [code]
  (let [[row col] (split-at 7 code)]
    (+ (* (decode row) 8) (decode col))))

;; part 1
(apply max (map decode-seat input)) ; 878

;; part 2
(->> (map decode-seat input)
     (sort)
     (partition 2 1)
     (some (fn [[lo hi]] (when (= (+ lo 2) hi) (inc lo))))) ; 504
