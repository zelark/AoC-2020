(ns zelark.aoc-2020.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 16: Ticket Translation ---
;; https://adventofcode.com/2020/day/16

(def input (slurp (io/resource "input_16.txt")))

(defn parse-long [n]
  (Long/parseLong n))

(defn parse-ticket [ticket]
  (->> (str/split ticket #",")
       (map parse-long)))

(defn parse-input [input]
 (let [rules (->> (re-seq #"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)" input)
                  (map (fn [[_ field a b c d]]
                         [field [(parse-long a) (parse-long b)] [(parse-long c) (parse-long d)]])))
       tikets (->> (re-find #"nearby tickets:\n([\d,\n]+)" input)
                   (second)
                   (str/split-lines)
                   (map parse-ticket))
       my-ticket (->> (re-find #"your ticket:\n([\d,]+)" input)
                      (second)
                      (parse-ticket))]
   {:rules rules :tickets tikets :my-ticket my-ticket}))

(defn valid? [[_ [a b] [c d]] number]
  (or (<= a number b)
      (<= c number d)))

(defn find-invalid [rules ticket]
  (keep #(when-not (some (fn [rule] (valid? rule %)) rules) %) ticket))

;; part 1
(let [{:keys [rules tickets]} (parse-input input)]
  (->> (mapcat #(find-invalid rules %) tickets)
       (apply +))) ; 20013

;; part 2
(defn find-fields [rules field]
  (->> (filter (fn [rule] (every? #(valid? rule %) field)) rules)
       (map first)))

(defn clear-fields [fields]
  (let [n (count fields)
        singles (fn [fields] (reduce #(if (== (count %2) 1) (conj %1 (first %2)) %1) #{} fields))]
    (loop [fields fields
           found  (singles fields)]
      (if (== (count found) n)
        (map first fields)
        (let [fields' (map (fn [fs] (if (== (count fs) 1) fs (remove found fs))) fields)]
          (recur fields' (singles fields')))))))

(let [{:keys [rules my-ticket tickets]} (parse-input input)]
  (->> (remove #(seq (find-invalid rules %)) tickets)
       (apply map vector)
       (map #(find-fields rules %))
       (clear-fields)
       ((fn [fields] (zipmap fields my-ticket)))
       (filter (fn [[k _]] (str/starts-with? k "departure")))
       (vals)
       (apply *))) ; 5977293343129
