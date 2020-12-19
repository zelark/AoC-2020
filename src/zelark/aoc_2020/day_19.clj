(ns zelark.aoc-2020.day-19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 19: Monster Messages ---
;; https://adventofcode.com/2020/day/19

(def input (slurp (io/resource "input_19.txt")))

(defn parse-rule [rule]
  (let [[id content] (str/split rule #":")]
    [(Long. id)
     (if-let [final (re-find #"a|b" content)]
       final
       (read-string (str "(" content ")")))]))

(defn parse-input [input]
  (let [[rules messages] (str/split input #"\R\R")]
    {:rules (->> (str/split-lines rules)
                 (map parse-rule)
                 (into {}))
     :messages (str/split-lines messages)}))

(defn patch-rules [rules]
  (-> rules
      (assoc 8  '(42 | 42 8))
      (assoc 11 '(42 31 | 42 11 31))))

;; Java's regexs do not support recursion.
;; We limit recursive calls with depth.
(defn post-rules [rules start depth]
  (if (zero? depth)
    ""
    (if (list? (rules start))
      (let [rule (apply str (map (fn [x]
                                   (if (number? x)
                                     (post-rules rules x (cond-> depth (== start x) dec))
                                     x))
                                 (rules start)))]
        (str "(?:" rule ")"))
      (rules start))))

;; part 1
(let [{:keys [rules messages]} (parse-input input)
      pattern (re-pattern (post-rules rules 0 1))]
  (count (filter #(re-matches pattern %) messages))) ; 250

;; part 2
(let [{:keys [rules messages]} (parse-input input)
      rules' (patch-rules rules)
      pattern (re-pattern (post-rules rules' 0 5))]  ; five is enough
  (count (filter #(re-matches pattern %) messages))) ; 359
