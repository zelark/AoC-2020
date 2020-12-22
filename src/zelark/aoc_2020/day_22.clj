(ns zelark.aoc-2020.day-22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 22: Crab Combat ---
;; https://adventofcode.com/2020/day/22

(def input (slurp (io/resource "input_22.txt")))

(defn queue [coll] (into clojure.lang.PersistentQueue/EMPTY coll))

(defn parse-deck [s]
  (->> (str/split-lines s)
       (rest)
       (map #(Long/parseLong %))
       (queue)))

(defn parse-input [input]
  (let [[deck1 deck2] (str/split input #"\R\R")]
    [(parse-deck deck1) (parse-deck deck2)]))

(defn score [{deck :deck}]
  (->> (reverse deck)
       (map * (rest (range)))
       (apply +)))

;; part 1
(defn combat [deck1 deck2]
  (cond
    (empty? deck1) {:deck deck2 :winner :crab}
    (empty? deck2) {:deck deck1 :winer  :me}
    :else
    (let [[card1 deck1] ((juxt peek pop) deck1)
          [card2 deck2] ((juxt peek pop) deck2)]
      (if (< card1 card2)
        (recur deck1 (conj deck2 card2 card1))
        (recur (conj deck1 card1 card2) deck2)))))

(->> (parse-input input) (apply combat) score) ; 30197

;; part 2
(defn recursive-combat [deck1 deck2]
  (loop [deck1 deck1 deck2 deck2 history #{}]
    (cond
      (history [deck1 deck2]) {:deck deck1 :winner :me}
      (empty? deck1) {:deck deck2 :winner :crab}
      (empty? deck2) {:deck deck1 :winner :me}
      :else
      (let [history (conj history [deck1 deck2])
            [card1 deck1] ((juxt peek pop) deck1)
            [card2 deck2] ((juxt peek pop) deck2)]
        (if (and (<= card1 (count deck1))
                 (<= card2 (count deck2)))
          (if (= (:winner (recursive-combat (queue (take card1 deck1))
                                            (queue (take card2 deck2))))
                 :me)
            (recur (conj deck1 card1 card2) deck2 history)
            (recur deck1 (conj deck2 card2 card1) history))
          (if (< card1 card2)
            (recur deck1 (conj deck2 card2 card1) history)
            (recur (conj deck1 card1 card2) deck2 history)))))))

(->> (parse-input input) (apply recursive-combat) score) ; 34031
