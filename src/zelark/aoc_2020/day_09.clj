(ns zelark.aoc-2020.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 9: Encoding Error ---
;; https://adventofcode.com/2020/day/9

(def input (slurp (io/resource "input_09.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(Long/parseLong %))))

(defn invalid [numbers]
  (let [[num window] ((juxt last (comp set butlast)) numbers)]
    (when (not-any? #(window (- num %)) window)
      num)))

;; part 1
(->> (parse-input input)
     (partition 26 1) ; 25 numbers + 1
     (some invalid))  ; 41682220

;; part 2
(defn find-encryption-weakness [target numbers]
  (let [sums (take-while #(<= % target) (reductions + numbers))]
    (when (== target (last sums))
      (->> (take (count sums) numbers)
           (apply (juxt min max))
           (apply +)))))

(->> (parse-input input)
     (iterate next)
     (take-while seq)
     (some (partial find-encryption-weakness 41682220))) ; 5388976
