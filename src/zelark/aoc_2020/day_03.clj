(ns zelark.aoc-2020.day-03
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/reader (io/resource "input_03.txt"))
       (line-seq)))

(defn tree? [row x]
  (= (nth row (mod x (count row))) \#))

(defn check-slope [input [right down]]
  (first
   (reduce (fn [[n x] row]
             [(cond-> n (tree? row x) inc)
              (+ x right)])
           [0 right]
           (rest (take-nth down input)))))

;; part 1
(check-slope input [3 1]) ;; 274

;; part 2
(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(->> (map (partial check-slope input) slopes)
     (apply *)) ;; 6050183040
