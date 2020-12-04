(ns zelark.aoc-2020.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 4: Passport Processing ---
;; https://adventofcode.com/2020/day/4

(def input (slurp (io/resource "input_04.txt")))

(defn parse-entry [entry]
  (into {} (map #(str/split % #":") (str/split entry #"\s"))))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map parse-entry)))

;; part 1
(def required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn has-required-fileds? [rec]
  (every? (partial contains? rec) required-fields))

(->> (parse-input input) (filter has-required-fileds?) count) ;; 208

(defn in-range? [min max num]
  (and (re-matches #"\d+" num)
       (<= min (Long/parseLong num) max)))

;; part 2
(def rules
  {:byr (partial in-range? 1920 2002)
   :iyr (partial in-range? 2010 2020)
   :eyr (partial in-range? 2020 2030)
   :hgt (fn [x]
           (let [[_ height unit] (re-find #"^(\d+)(in|cm)$" x)]
             (case unit
               "cm" (in-range? 150 193 height)
               "in" (in-range? 59 76 height)
               false)))
   :hcl (partial re-find #"^#[0-9a-f]{6}$")
   :pid (partial re-find #"^\d{9}$")
   :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   :cid (constantly true)})

(defn valid? [rec]
  (every? #((rules (-> % key keyword)) (val %)) rec))

(->> (parse-input input)
     (filter has-required-fileds?)
     (filter valid?)
     count) ;; 167
