(ns zelark.aoc-2020.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

;; --- Day 18: Operation Order ---
;; https://adventofcode.com/2020/day/18

(def input (slurp (io/resource "input_18.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(read-string (str "(" % ")")))))

(defn eval-1 [[x & xs]]
  (reduce (fn [acc [op arg]] ((resolve op) acc arg)) x (partition 2 xs)))

(defn eval-2 [[x & xs]]
  (let [ev (fn ev [acc [[op arg] & rst]]
             (case op
               + (recur (+ acc arg) rst)
               * (* acc (ev arg rst))
               acc))]
    (ev x (partition 2 xs))))

(defn calc [eval-fn line]
  (walk/postwalk #(cond->> % (list? %) eval-fn) line))

;; part 1
(->> (parse-input input)
     (map (partial calc eval-1))
     (apply +)) ; 3885386961962

;; part 2
(->> (parse-input input)
     (map (partial calc eval-2))
     (apply +)) ; 112899558798666
