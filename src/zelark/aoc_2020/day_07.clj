(ns zelark.aoc-2020.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 7: Handy Haversacks ---
;; https://adventofcode.com/2020/day/7

(def input (slurp (io/resource "input_07.txt")))

(defn parse-entry [entry]
  (let [p  (re-find #"\w+ \w+" entry)
        cs (->> (re-seq #"(\d+) (\w+ \w+)" entry)
                (reduce (fn [m [_ v k]] (assoc m k (Long/parseLong v))) {}))]
    [p cs]))

(def bags (into {} (map parse-entry (str/split-lines input))))

;; part 1
(defn find-outer-bags [bags bag]
  (let [found-bags (keys (filter #(contains? (val %) bag) bags))]
    (into (set found-bags) (mapcat #(find-outer-bags bags %) found-bags))))

(count (find-outer-bags bags "shiny gold")) ; 103

;; part 2
(defn count-bags [bags [bag n]]
  (* n (apply + 1 (map #(count-bags bags %) (get bags bag)))))

(dec (count-bags bags ["shiny gold" 1])) ; 1469
