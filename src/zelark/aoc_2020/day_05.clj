(ns zelark.aoc-2020.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 5: Binary Boarding ---
;; https://adventofcode.com/2020/day/5

(def input (slurp (io/resource "input_05.txt")))

(defn decode [code]
  (-> (str/escape code {\F 0 \L 0 \B 1 \R 1})
      (Long/parseLong 2)))

;; part 1
(apply max (map decode (str/split-lines input))) ; 878

;; part 2
(->> (map decode (str/split-lines input))
     (sort)
     (partition 2 1)
     (some (fn [[lo hi]] (when (= (+ lo 2) hi) (inc lo))))) ; 504
