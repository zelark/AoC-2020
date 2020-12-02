(ns zelark.aoc-2020.day-02
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/reader (io/resource "input_02.txt"))
       (line-seq)
       (mapcat (partial re-seq #"(\d+)-(\d+)\s([a-z]):\s([a-z]+)"))
       (map (fn [[_ from to ch pass]] [(Long/parseLong from)
                                       (Long/parseLong to)
                                       (first ch)
                                       pass]))))

;; part 1
(defn valid-pass? [[from to ch pass]]
  (let [cnt (reduce #(if (= %2 ch) (inc %1) %1) 0 pass)]
    (<= from cnt to)))

(->> input
     (filter valid-pass?)
     count)

;; part 2
(->> input
     (filter (fn [[pos1 pos2 ch pass]]
               (let [a (nth pass (dec pos1))
                     b (nth pass (dec pos2))]
                 (cond
                   (and (= a ch) (not= b ch)) true
                   (and (= b ch) (not= a ch)) true
                   :else false))))
     count)
