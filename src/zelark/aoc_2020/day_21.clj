(ns zelark.aoc-2020.day-21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 21: Allergen Assessment ---
;; https://adventofcode.com/2020/day/21

(def input (slurp (io/resource "input_21.txt")))

(defn parse-item [line]
   (let [[ingredients allergens] (str/split line #"contains")
         ingredients (set (re-seq #"\w+" ingredients))
         allergens   (re-seq #"\w+" allergens)]
     (zipmap allergens (repeat ingredients))))

(defn parse-input [input]
  (let [food (->> (str/split-lines input)
                  (map parse-item))]
    {:allergens   (reduce (partial merge-with set/intersection) food)
     :ingrediends (->> (map (comp val first ) food) (reduce into []) frequencies)}))

(defn narrow [coll]
  (loop [[[k v] & xs] (sort-by (comp count val) coll)
         result {}]
    (if (== (count result) (count coll))
      result
      (recur (sort-by (comp count val) xs)
             (assoc result (first (remove result v)) k)))))

;; part 1
(let [{:keys [allergens ingrediends]} (parse-input input)]
  (->> (reduce #(apply dissoc %1 %2) ingrediends (vals allergens))
       (vals)
       (apply +))) ; 1885

;; part 2
(let [{:keys [allergens]} (parse-input input)]
  (->> (narrow allergens)
       (sort-by val)
       (map first)
       (str/join ","))) ; fllssz,kgbzf,zcdcdf,pzmg,kpsdtv,fvvrc,dqbjj,qpxhfp
