(ns zelark.aoc-2020.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 4: Passport Processing ---
;; https://adventofcode.com/2020/day/4
(def input (slurp (io/resource "input_04.txt")))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"})

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map #(str/split % #"[\s\n]"))
       (map (partial map #(str/split % #":")))))

(defn has-required-fileds? [rec]
  (let [diff (set/difference required-fields (set (map first rec)))]
    (or (empty? diff)
        (= diff #{"cid"}))))

;; part 1
(count (filter has-required-fileds? (parse-input input))) ;; 208

(def rules
  {"byr" [#(re-find #"^\d{4}$" %) #(<= 1920 (Long/parseLong %) 2002)]
   "iyr" [#(re-find #"^\d{4}$" %) #(<= 2010 (Long/parseLong %) 2020)]
   "eyr" [#(re-find #"^\d{4}$" %) #(<= 2020 (Long/parseLong %) 2030)]
   "hgt" [#(re-find #"^(\d+)(in|cm)$" %)
          (fn [x]
           (let [[_ height unit] (re-find #"(\d+)(in|cm)" x)
                 height          (Long/parseLong height)]
             (if (= unit "cm")
               (<= 150 height 193)
               (<= 59  height 76))))]
   "hcl" [#(re-find #"^#[0-9a-f]{6}$" %)]
   "ecl" [#(re-find #"^(amb|blu|brn|gry|grn|hzl|oth)$" %)]
   "pid" [#(re-find #"^\d{9}$" %)]
   "cid" [identity]})

;; part 2
(defn valid? [rec]
  (let [validate (fn [[field value]]
                   ((apply every-pred (rules field)) value))]
   (every? validate rec)))

(->> (parse-input input)
     (filter has-required-fileds?)
     (filter valid?)
     (count)) ;; 167
