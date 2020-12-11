(ns zelark.aoc-2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 11: Seating System ---
;; https://adventofcode.com/2020/day/11

(def input (slurp (io/resource "input_11.txt")))

(defn parse-input [input target]
  (->> (str/split-lines input)
       (mapcat (fn [y row] (keep-indexed (fn [x ch] (when (= ch target) [x y])) row))
               (range))))

(def seats (set (parse-input input \L)))
(def floor (set (parse-input input \.))) ; needed for part 2

(defn build-neighbours [find-fn seats]
  (reduce #(assoc %1 %2 (find-fn seats %2)) {} seats))

(defn step [seats neighbours tolerance occupied?]
  (set (keep (fn [seat]
               (let [n (->> (neighbours seat) (filter occupied?) count)]
                 (when (or (and (not (occupied? seat)) (zero? n))
                           (and (occupied? seat) (< n tolerance)))
                   seat)))
             seats)))

(defn solve [step]
  (->> (iterate step #{})
       (partition 2 1)
       (drop-while #(apply not= %))
       ffirst count))

;; part 1
(defn find-near-neighbours [seats [x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)
        :let [seat [(+ x dx) (+ y dy)]]
        :when (seats seat)]
    seat))

(let [neighbours (build-neighbours find-near-neighbours seats)]
  (solve (partial step seats neighbours 4))) ; 2359

;; part 2
(defn find-first-neighbours [floor seats [x y]]
  (->> (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
         (->> (iterate (fn [[x y]] [(+ x dx) (+ y dy)]) [x y])
              rest
              (drop-while floor)
              first seats))
       (remove nil?)))

(let [find-fn (partial find-first-neighbours floor)
      neighbours (build-neighbours find-fn seats)]
  (solve (partial step seats neighbours 5))) ; 2131
