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
            [dx dy])))

(defn go-to-tile [from path]
  (->> (map directions path)
       (reduce (partial mapv +) from)))

;; part 1
(def black-tiles (->> (parse-input input)
                      (map (partial go-to-tile [0 0]))
                      (frequencies)
                      (keep (fn [[loc n]] (when (= n 1) loc)))
                      (set)))

(count black-tiles) ; 427

;; part 2
(defn neighbours [[x y]]
  (for [dy [-1 0 1] dx (if (zero? dy) [2 -2] [-1 1])]
    [(+ x dx) (+ y dy)]))

(defn step [black-tiles]
  (set (for [[loc n] (frequencies (mapcat neighbours black-tiles))
             :when (or (= n 2) (and (<= 1 n 2) (black-tiles loc)))]
        loc)))

(->> (iterate step black-tiles)
     (drop 100)
     (first)
     (count)) ; 3837
