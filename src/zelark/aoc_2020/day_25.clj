(ns zelark.aoc-2020.day-25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 25: Combo Breaker ---
;; https://adventofcode.com/2020/day/25

(def input (slurp (io/resource "input_25.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(Long/parseLong %))))

(def ^:const module 20201227)

(defn transform [^long subject-number]
  (iterate #(mod (* ^long % subject-number) module) 1))

(defn find-loop-size [^long public-key]
  (count (take-while #(not= ^long % public-key) (transform 7))))

(defn mod-pow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

;; part 1
(let [[card-pubkey door-pubkey] (parse-input input)
      loop-size (find-loop-size card-pubkey)]
    (mod-pow door-pubkey loop-size module)) ; 12181021

