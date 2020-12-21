(ns zelark.aoc-2020.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 16: Ticket Translation ---
;; https://adventofcode.com/2020/day/16

(def input (slurp (io/resource "input_16.txt")))

(defn parse-longs [s]
  (->> (re-seq #"\d+" s)
       (mapv #(Long/parseLong %))))

(defn parse-input [input]
 (let [[rules my-ticket tikets] (str/split input #"\R\R")
       rules (->> (str/split-lines rules)
                  (map #(str/split % #":"))
                  (map (fn [[field ranges]] [field (partition 2 (parse-longs ranges))])))
       tikets (->> (str/split-lines tikets)
                   (rest)
                   (map parse-longs))
       my-ticket (parse-longs my-ticket)]
   {:rules rules :tickets tikets :my-ticket my-ticket}))

(defn valid? [[_ [[a b] [c d]]] number]
  (or (<= a number b)
      (<= c number d)))

(defn find-invalid [rules ticket]
  (keep #(when-not (some (fn [rule] (valid? rule %)) rules) %) ticket))

;; part 1
(let [{:keys [rules tickets]} (parse-input input)]
  (->> (flatten tickets)
       (remove (fn [n] (some #(valid? % n) rules)))
       (apply +))) ; 20013

;; part 2
(defn find-fields [rules field]
  (->> (filter (fn [rule] (every? #(valid? rule %) field)) rules)
       (map first)
       (set)))

(defn narrow [coll]
  (loop [[[k v] & xs] (sort-by (comp count second) coll)
         result {}]
    (if (nil? k)
      result
      (recur (->> (map (fn [[kn vn]] [kn (disj vn (first v))]) xs)
                  (sort-by (comp count second)))
             (assoc result (first v) k)))))

(let [{:keys [rules my-ticket tickets]} (parse-input input)]
  (->> (remove #(seq (find-invalid rules %)) tickets)
       (apply map vector)
       (map-indexed #(-> [%1 (find-fields rules %2)]))
       (narrow)
       (filter (fn [[field]] (str/starts-with? field "departure")))
       (map (fn [[_ idx]] (my-ticket idx)))
       (apply *))) ; 5977293343129
