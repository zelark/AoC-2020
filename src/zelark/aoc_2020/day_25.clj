(ns zelark.aoc-2020.day-25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 25: Combo Breaker ---
;; https://adventofcode.com/2020/day/25

(def input (slurp (io/resource "input_25.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(Long/parseLong %))))

(defn transform [^long subject-number]
  (iterate #(rem (* ^long % subject-number) 20201227) 1))

(defn find-loop-size [^long public-key]
  (some (fn [[^long n ^long key]] (when (== key public-key) n))
        (map-indexed vector (transform 7))))

(defn find-encryption-key [public-key loop-size]
  (->> (transform public-key)
       (drop loop-size)
       (first)))

;; part 1
(let [[card-pubkey door-pubkey] (parse-input input)]
  (->> (find-loop-size card-pubkey)
       (find-encryption-key door-pubkey))) ; 12181021
