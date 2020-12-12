(ns zelark.aoc-2020.day-12
  (:require [clojure.java.io :as io]))

;; --- Day 12: Rain Risk ---
;; https://adventofcode.com/2020/day/12

(def input (slurp (io/resource "input_12.txt")))

(defn parse-input [input]
  (->> (re-seq #"([NSEWLRF])(\d+)" input)
       (map (fn [[_ action value]] [(keyword action) (Long/parseLong value)]))))

(def directions [:west :north :east :south])
(def actions (zipmap [:W :N :E :S :L :R] (conj directions - +)))

(defn solve [step init instractions]
  (let [{:keys [east north west south]} (reduce step init instractions)]
    (+ (Math/abs (- north south))
       (Math/abs (- east  west)))))

;; part 1
(defn rotate [direction sign angle]
  (let [idx (sign (.indexOf directions direction) (quot angle 90))]
    (nth directions (mod idx 4))))

(defn step [{:keys [direction] :as state} [action value]]
  (case action
    :F            (update state direction (fnil + 0) value)
    (:N :S :W :E) (update state (actions action) (fnil + 0) value)
    (:L :R)       (update state :direction #(rotate % (actions action) value))))

(solve step {:direction :east} (parse-input input)) ; 1319

;; part 2
(defn rotate-waypoint [waypoint sign angle]
  (reduce-kv (fn [m k v] (assoc m (rotate k sign angle) v)) {} waypoint))

(defn step-with-wp [{:keys [waypoint] :as state} [action value]]
  (let [times #(reduce-kv (fn [m k v] (assoc m k (* v value))) {} %)]
    (case action
      :F            (merge-with (fnil + 0) state (times waypoint))
      (:N :S :W :E) (update-in state [:waypoint (actions action)] (fnil + 0) value)
      (:L :R)       (update state :waypoint #(rotate-waypoint % (actions action) value)))))

(solve step-with-wp {:waypoint {:east 10 :north 1}} (parse-input input)) ; 62434
