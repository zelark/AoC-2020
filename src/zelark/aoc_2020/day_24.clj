(ns zelark.aoc-2020.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 24: Lobby Layout ---
;; https://adventofcode.com/2020/day/24

(def input (slurp (io/resource "input_24.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-seq #"e|se|sw|w|nw|ne" %))))

(def directions
  (zipmap ["nw" "ne" "e" "w" "sw" "se"]
          (for [dy [-1 0 1] dx (if (zero? dy) [2 -2] [-1 1])]
            (fn [[x y]] [(+ x dx) (+ y dy)]))))

(defn path->tile [path]
  (reduce #((directions %2) %1) [0 0] path))

;; part 1
(def black-tiles (->> (parse-input input)
                      (map path->tile)
                      (reduce (fn [bs t] (if (bs t) (disj bs t) (conj bs t))) #{})))

(count black-tiles) ; 427

;; part 2
(defn neighbours [tile]
  (map #(% tile) (vals directions)))

(defn step [tiles]
  (set (for [[tile n] (frequencies (mapcat neighbours tiles))
             :when (or (= n 2) (and (<= 1 n 2) (tiles tile)))]
        tile)))

(->> (iterate step black-tiles) (drop 100) first count) ; 3837
