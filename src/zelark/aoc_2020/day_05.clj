(ns zelark.aoc-2020.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 5: Binary Boarding ---
;; https://adventofcode.com/2020/day/5

(def input (str/split-lines (slurp (io/resource "input_05.txt"))))

(defn decode [lo hi s]
  (if-let [c (first s)]
    (case c
      (\F \L) (recur lo (+ lo (quot (- hi lo) 2)) (next s))
      (\B \R) (recur (+ lo (inc (quot (- hi lo) 2))) hi (next s)))
    lo))

(def decode-row (partial decode 0 127))
(def decode-col (partial decode 0 7))

(defn decode-seat [code]
  (let [[row col] (split-at 7 code)]
    (+ (* (decode-row row) 8) (decode-col col))))

;; part 1
(apply max (map decode-seat input)) ; 878

;; part 2
(->> (map decode-seat input)
     (sort)
     (partition 2 1)
     (some (fn [[lo hi]] (when (= (+ lo 2) hi) (inc lo))))) ; 504
