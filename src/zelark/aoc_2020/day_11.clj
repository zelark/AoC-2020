(ns zelark.aoc-2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 11: Seating System ---
;; https://adventofcode.com/2020/day/11

(def input (slurp (io/resource "input_11.txt")))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapcat (fn [y row] (keep-indexed (fn [x ch] (when (= ch \L) [x y])) row))
               (range))))

(def seats (set (parse-input input)))

(defn build-neighbours [find-fn seats]
  (reduce #(assoc %1 %2 (find-fn seats %2)) {} seats))

(defn step [seats neighbours limit occupied?]
  (set (keep (fn [seat]
               (let [n (->> (neighbours seat) (filter occupied?) count)]
                 (when (or (and (not (occupied? seat)) (zero? n))
                           (and (occupied? seat) (< n limit)))
                   seat)))
             seats)))

(defn solve [step]
  (loop [occupied #{}]
    (let [occupied' (step occupied)]
      (if (= occupied' occupied)
        (count occupied)
        (recur occupied')))))

;; part 1
(defn find-neighbours [seats [x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)
        :let [seat [(+ x dx) (+ y dy)]]
        :when (seats seat)]
    seat))

(solve (partial step seats (build-neighbours find-neighbours seats) 4)) ; 2359

;; part 2
(defn find-neighbours-2 [w h seats [x y]]
  (->> (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
         (loop [x' (+ x dx) y' (+ y dy)]
           (when (and (<= 0 x' w) (<= 0 y' h))
             (if (contains? seats [x' y'])
               [x' y']
               (recur (+ x' dx) (+ y' dy))))))
       (remove nil?)))

(let [[w _] (apply max-key first seats)
      [_ h] (apply max-key second seats)
      find-fn (partial find-neighbours-2 w h)
      neighbours (build-neighbours find-fn seats)]
  (solve (partial step seats neighbours 5))) ; 2131
