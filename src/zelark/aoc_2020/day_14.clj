(ns zelark.aoc-2020.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 14: Docking Data ---
;; https://adventofcode.com/2020/day/14

(def input (slurp (io/resource "input_14.txt")))

(defn parse-mask [mask]
  (->> (zipmap (range) (reverse mask))))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-matches #"(?:(mask) = ([X10]+)|(mem)\[(\d+)\] = (\d+))" %))
       (map #(remove nil? %))
       (map (fn [[_ cmd a b ]] (case cmd "mask" [:mask (parse-mask a)] "mem" [:mem (Long/parseLong a) (Long/parseLong b)])))))

(def demo-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(parse-input demo-input)
(parse-input input)

(defn apply-mask [mask number]
  (reduce-kv (fn [num idx bit]
               (case bit
                 \0 (bit-clear num idx)
                 \1 (bit-set   num idx)
                 \X num))
           number mask))

(defn run-program [code]
  (loop [commands code mask {} mem {}]
    (if-let [[cmd a b] (first commands)]
      (case cmd
        :mask (recur (next commands) a mem)
        :mem  (recur (next commands) mask (assoc mem a (apply-mask mask b))))
      mem)))

(apply + (vals (run-program (parse-input input)))) ; 13476250121721

;; part 2
(def demo-input-2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(defn apply-floating [floating a b]
  (reduce (fn [x [nf nm]]
            (if (bit-test b nm)
              (bit-set x nf)
              (bit-clear x nf))) a (map vector floating (range))))

(defn apply-mask-v2 [mask number]
  (let [floating (keys (filter #(-> % val (= \X)) mask))
        num (reduce-kv (fn [num idx bit]
                 (case bit
                   (\0 \X) num
                   \1 (bit-set num idx)))
               number mask)]
    (map (partial apply-floating floating) (repeat num) (range 0 (apply * (repeat (count floating) 2))))))

(defn run-program-v2 [code]
  (loop [commands code mask {} mem {}]
    (if-let [[cmd a b] (first commands)]
      (case cmd
        :mask (recur (next commands) a mem)
        :mem  (recur (next commands) mask (reduce #(assoc %1 %2 b) mem (apply-mask-v2 mask a))))
      mem)))

(apply + (vals (run-program-v2 (parse-input input)))) ; 4463708436768

