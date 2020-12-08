(ns zelark.aoc-2020.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 8: Handheld Halting ---
;; https://adventofcode.com/2020/day/8

(def input (slurp (io/resource "input_08.txt")))

(defn parse-instraction [inst]
  (let [[op arg] (str/split inst #" ")]
    [(keyword op) (Long/parseLong arg)]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv parse-instraction)))

(defn run-code [code]
  (let [history (atom #{})]
    (loop [acc 0 ip 0]
      (let [[op arg] (nth code ip [:hlt 0])]
        (if (contains? @history ip)
          {:inf-loop acc}
          (do (swap! history conj ip)
              (case op
                :hlt {:halt acc}
                :nop (recur acc (inc ip))
                :acc (recur (+ acc arg) (inc ip))
                :jmp (recur acc (+ ip arg)))))))))

;; part 1
(-> input parse-input run-code :inf-loop) ; 1501

;; part 2
(defn fix-code [code]
  (fn [i [op _]]
    (when (#{:nop :jmp} op)
      (update-in code [i 0] {:jmp :nop, :nop :jmp}))))

(let [code (parse-input input)]
  (->> (keep-indexed (fix-code code) code)
       (map run-code)
       (some :halt))) ; 509
