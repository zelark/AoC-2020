(ns zelark.aoc-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 10: Adapter Array ---
;; https://adventofcode.com/2020/day/10

(def input (slurp (io/resource "input_10.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(Long/parseLong %))))

(def parsed-input (parse-input input))
(def charging-outlet 0)
(def built-in (+ (apply max parsed-input) 3))
(def adapters (sort (conj parsed-input charging-outlet built-in)))

;; part 1
(->> (for [adapter adapters
           :let [jolts (set (map + [1 2 3] (repeat 3 adapter)))
                 found (some jolts adapters)]
           :when found]
       (- found adapter))
     (frequencies)
     (vals)
     (apply +)) ; 2400

;; part 2
(defn find-adapters [adapter adapters]
  (let [jolts (set (map + [1 2 3] (repeat 3 adapter)))]
    (filter jolts adapters)))

(def count-arranges
  (memoize
   (fn [start]
     (if-let [found (seq (find-adapters start adapters))]
       (apply + (dec (count found)) (map count-arranges found))
       0))))

(inc (count-arranges 0)) ; 338510590509056
