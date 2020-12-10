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
    (conj adapters 0 (+ (last adapters) 3))))

(def adapters (parse-input input))

(defn find-next [adapter adapters]
  (seq (filter adapters (map + [1 2 3] (repeat 3 adapter)))))

;; part 1
(->> adapters
     (keep #(when-first [found (find-next % adapters)] (- found %)))
     (frequencies)
     (vals)
     (apply *)) ; 2400

;; part 2
(def count-arranges
  (memoize
   (fn [start]
     (if-let [found (find-next start adapters)]
       (apply + (dec (count found)) (map count-arranges found))
       0))))

(inc (count-arranges 0)) ; 338510590509056
