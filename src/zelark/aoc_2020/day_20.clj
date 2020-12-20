(ns zelark.aoc-2020.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 20: Jurassic Jigsaw ---
;; https://adventofcode.com/2020/day/20

(def input (slurp (io/resource "input_20.txt")))

(defn parse-tile [tile]
  (let [[id & tile] (str/split-lines tile)
        id (Long. (re-find #"\d+" id))
        tile (mapv #(mapv identity %) tile)]
    [id tile]))

(defn parse-input [input]
  (->> (str/split input #"\R\R")
       (map parse-tile)
       (into {})))

(def tiles (parse-input input))

(defn rotate [tile]
  (let [h (count tile)
        w (count (first tile))]
    (->> (for [i (range w) j (range h)]
           (get-in tile [(- h j 1) i]))
         (partition h)
         (mapv vec))))

(defn flip [tile]
  (vec (reverse tile)))

(defn empty-image [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn edges [tile]
  {:top    (first tile)
   :bottom (peek tile)
   :left   (map first tile)
   :right  (map peek tile)})

(defn line-up? [edge a b]
  (let [opposite-edge ({:top :bottom :bottom :top :left :right :right :left} edge)]
    (= (edge (edges a)) (opposite-edge (edges b)))))

(defn find-adjecent [tile tiles]
    (for [edge [:top :right :bottom :left]]
      (some (fn [[id tile-b]]
              (or (some #(when (line-up? edge tile %) [id %]) (take 4 (iterate rotate tile-b)))
                  (some #(when (line-up? edge tile %) [id %]) (take 4 (iterate rotate (flip tile-b))))))
            tiles)))

(defn add-loc [adjecent [x y]]
  (keep (fn [[id loc]] (when (some? id) [id loc]))
        (map vector
             (map first adjecent)
             (for [[dx dy] [[0 -1] [1 0] [0 1] [-1 0]]] [(+ x dx) (+ y dy)]))))

(let [[start-id start-tile] (first tiles)]
  (loop [image-map {}
         tiles-map {start-id start-tile}
         rest-tiles (dissoc tiles start-id)
         queue [[start-id [0 0]]]]
    (if (seq queue)
      (let [[tile-id [x y]] (first queue)
            adjecent        (find-adjecent (tiles-map tile-id) rest-tiles)]
        (recur (assoc image-map [x y] tile-id)
               (into tiles-map adjecent)
               (apply dissoc rest-tiles (keep first adjecent))
               (into (rest queue) (add-loc adjecent [x y]))))
      (do
        (def image-map image-map)
        (def tiles-map tiles-map)))))

;; part 1
(->> (for [x (apply (juxt min max) (map first (keys image-map)))
           y (apply (juxt min max) (map second (keys image-map)))]
       (get image-map [x y]))
     (apply *)) ; 29293767579581

;; part 2
(defn cut-border [tile]
  (let [end (-> tile count dec)]
    (mapv #(subvec % 1 end) (subvec tile 1 end))))

(def image-ascii
  (->> (for [y (range -1 (inc 10))
             x (range -4 (inc 7))
             :let [tile-id (get image-map [x y])]]
         (cut-border (tiles-map tile-id)))
       (partition 12)
       (mapcat (fn [row] (reduce #(map into %1 %2) row)))
       (vec)))

(defn re-count [image pattern]
  (->> (map str/join image)
       (str/join \newline)
       (re-seq (re-pattern pattern))
       count))

(- (re-count image-ascii #"#")
   (* (re-count (rotate (rotate (rotate image-ascii))) #"#....##....##....###") 15)) ; 1989
