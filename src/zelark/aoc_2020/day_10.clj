(ns zelark.aoc-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 10: Adapter Array ---
;; https://adventofcode.com/2020/day/10

(def input (slurp (io/resource "input_10.txt")))

(defn parse-input [input]
  (let [adapters (->> (str/split-lines input)
                      (map #(Long/parseLong %))
                      (apply sorted-set))]
    (vec (conj adapters 0 (+ (last adapters) 3)))))

(def adapters (parse-input input))

;; part 1
(->> (frequencies (map - (rest adapters) adapters))
     vals
     (apply *)) ; 2400

;; part 2
(-> (reduce (fn [cnts n]
              (let [cnt (->> (map + (repeat n) [1 2 3])
                             (keep cnts)
                             (apply +))]
                (assoc cnts n cnt)))
            (sorted-map (peek adapters) 1)
            (rest (rseq adapters)))
    first val) ; 338510590509056
