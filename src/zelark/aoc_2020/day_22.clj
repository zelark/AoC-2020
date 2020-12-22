(ns zelark.aoc-2020.day-22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 22: Crab Combat ---
;; https://adventofcode.com/2020/day/22

(def input (slurp (io/resource "input_22.txt")))

(def queue clojure.lang.PersistentQueue/EMPTY)

(defn parse-deck [s]
  (->> (str/split-lines s)
       (rest)
       (map #(Long/parseLong %))
       (into queue)))

(defn parse-input [input]
  (let [[deck1 deck2] (str/split input #"\R\R")]
    [(parse-deck deck1) (parse-deck deck2)]))

(defn solve [play [deck1 deck2]]
  (->> (play deck1 deck2)
       :deck
       (reverse)
       (map * (rest (range)))
       (apply +)))

;; part 1
(defn play-game [deck1 deck2]
  (cond
    (empty? deck1) {:deck deck2}
    (empty? deck2) {:deck deck1}
    :else
    (let [card1 (peek deck1)
          card2 (peek deck2)]
      (if (< card1 card2)
        (recur (pop deck1) (conj (pop deck2) card2 card1))
        (recur (conj (pop deck1) card1 card2) (pop deck2))))))

(solve play-game (parse-input input)) ; 30197

;; part 2
(defn play-recursive-game [deck1 deck2]
  (loop [deck1 deck1 deck2 deck2 history #{}]
    (cond
      (history [deck1 deck2]) {:deck deck1 :winner :player1}
      (empty? deck1) {:deck deck2 :winner :player2}
      (empty? deck2) {:deck deck1 :winner :player1}
      :else
      (let [card1    (peek deck1)
            card2    (peek deck2)
            deck1'   (pop deck1)
            deck2'   (pop deck2)
            history' (conj history [deck1 deck2])]
        (if (and (<= card1 (count deck1'))
                 (<= card2 (count deck2')))
          (if (= (:winner (play-recursive-game (into queue (take card1 deck1'))
                                               (into queue (take card2 deck2'))))
                 :player1)
            (recur (conj deck1' card1 card2) deck2' history')
            (recur deck1' (conj deck2' card2 card1) history'))
          (if (< card1 card2)
            (recur deck1' (conj deck2' card2 card1) history')
            (recur (conj deck1' card1 card2) deck2' history')))))))

(solve play-recursive-game (parse-input input)) ; 34031
