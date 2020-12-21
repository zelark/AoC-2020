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
  (let [foods (->> (str/split-lines input)
                   (map parse-item))]
    {:allergens   (reduce (partial merge-with set/intersection) foods)
     :ingrediends (->> (map (comp val first) foods)
                       (reduce into [])
                       (frequencies))}))

(defn narrow [coll]
  (loop [[[k v] & xs] (sort-by (comp count second) coll)
         result {}]
    (if (nil? k)
      result
      (recur (->> (map (fn [[kn vn]] [kn (disj vn (first v))]) xs)
                  (sort-by (comp count second)))
             (assoc result (first v) k)))))

;; part 1
(let [{:keys [allergens ingrediends]} (parse-input input)]
  (->> (vals allergens)
       (reduce (partial apply dissoc) ingrediends)
       (vals)
       (apply +))) ; 1885

;; part 2
(let [{:keys [allergens]} (parse-input input)]
  (->> (narrow allergens)
       (sort-by val)
       (map first)
       (str/join ","))) ; fllssz,kgbzf,zcdcdf,pzmg,kpsdtv,fvvrc,dqbjj,qpxhfp
