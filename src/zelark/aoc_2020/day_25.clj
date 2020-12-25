(ns zelark.aoc-2020.day-25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 25: Combo Breaker ---
;; https://adventofcode.com/2020/day/25

(def input (slurp (io/resource "input_25.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(Long/parseLong %))))

(defn secret-loop [^long subject-number]
  (iterate #(rem (* ^long % subject-number) 20201227) subject-number))

(defn find-loop-size [^long public-key]
  (some (fn [[^long n ^long key]] (when (== key public-key) n))
        (map-indexed vector (secret-loop 7))))

(defn find-secret-key [public-key loop-size]
  (->> (secret-loop public-key)
       (drop loop-size)
       first))

(let [[card-key door-key] (parse-input input)]
  (->> (find-loop-size card-key)
       (find-secret-key door-key))) ; 12181021
