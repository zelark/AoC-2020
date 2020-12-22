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

(defn valid-value? [[_ [[a b] [c d]]] number]
  (or (<= a number b)
      (<= c number d)))

;; part 1
(let [{:keys [rules tickets]} (parse-input input)]
  (->> (flatten tickets)
       (remove (fn [n] (some #(valid-value? % n) rules)))
       (apply +))) ; 20013

;; part 2
(defn narrow [coll]
  (loop [[[k v] & xs] (sort-by (comp count second) coll)
         result {}]
    (if (nil? k)
      result
      (recur (->> (map (fn [[kn vn]] [kn (disj vn (first v))]) xs)
                  (sort-by (comp count second)))
             (assoc result k (first v))))))

(defn valid-ticket? [rules ticket]
  (every? #(some (fn [rule] (valid-value? rule %)) rules) ticket))

(defn valid-cols [rule cols]
  (set (keep-indexed (fn [n values]
                       (when (every? #(valid-value? rule %) values) n))
                     cols)))

(let [{:keys [rules tickets my-ticket]} (parse-input input)
      columns (->> (filter #(valid-ticket? rules %) tickets)
                   (apply map vector))]
  (->> rules
       (reduce (fn [m [field :as rule]]
                 (assoc m field (valid-cols rule columns))) {})
       (narrow)
       (filter (fn [[field]] (str/starts-with? field "departure")))
       (map (fn [[_ n]] (my-ticket n)))
       (apply *))) ; 5977293343129
