(ns zelark.aoc-2020.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 2: Password Philosophy ---
;; https://adventofcode.com/2020/day/2

(def input (slurp (io/resource "input_02.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (partial re-find #"(\d+)-(\d+)\s(.):\s(.+)"))
       (map (fn [[_ min max ch pass]]
              [(Long/parseLong min) (Long/parseLong max) (first ch) pass]))))

;; part 1
(defn valid-pass? [[min max ch pass]]
  (<= min (count (filter #(= % ch) pass)) max))

(count (filter valid-pass? (parse-input input))) ; 586

;; part 2
(defn valid-pass2? [[min max ch pass]]
  (->> [min max] (map #(= (nth pass (dec %)) ch)) (apply not=)))

(count (filter valid-pass2? (parse-input input))) ; 352
