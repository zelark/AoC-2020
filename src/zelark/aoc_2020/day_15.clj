(ns zelark.aoc-2020.day-15
  (:require [clojure.java.io :as io]))

;; --- Day 15: Rambunctious Recitation ---
;; https://adventofcode.com/2020/day/15

(def input (slurp (io/resource "input_15.txt")))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (mapv #(Long/parseLong %))))

(defn solve [numbers ^long limit]
  (loop [spoken (zipmap numbers (rest (range)))
         turn   (count numbers)
         number (peek numbers)]
    (if (== turn limit)
      number
      (if-let [last-spoken (spoken number)]
        (recur (assoc spoken number turn) (inc turn) (- turn ^long last-spoken))
        (recur (assoc spoken number turn) (inc turn) 0)))))

;; part 1
(solve (parse-input input) 2020) ; 232

;; part 2
(solve (parse-input input) 30000000) ; 18929178

;; bonus: fast version
(defn solve-fast [numbers ^long limit]
  (let [spoken (int-array limit)
        _      (doseq [[^int i num] (map-indexed vector numbers)]
                 (aset spoken num (inc i)))]
    (loop [turn   (int (count numbers))
           number (peek numbers)]
      (if (== turn limit)
        number
        (let [last-spoken (aget spoken number)
              _           (aset spoken number turn)]
          (recur (unchecked-inc-int turn)
                 (cond->> last-spoken
                   (not= last-spoken 0) (unchecked-subtract-int turn))))))))

(solve-fast (parse-input input) 30000000) ; ~800 ms
