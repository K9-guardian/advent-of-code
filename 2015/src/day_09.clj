(ns day-09
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(def input (slurp "input/d9.txt"))

(defn parse-line [l]
  (let [[start end d] (->> l
                           (re-find #"(\w+) to (\w+) = (\d+)")
                           rest)
        d (Integer/parseInt d)]
    [{start {end d}} {end {start d}}]))

(defn path->distance [m p]
  (reduce #(+ %1 (get-in m %2))
          0
          (partition 2 1 p)))

(defn p1 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (reduce (partial apply merge-with merge) {}))
        paths (comb/permutations (keys m))]
    (->> paths
         (map (partial path->distance m))
         (apply min))))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (reduce (partial apply merge-with merge) {}))
        paths (comb/permutations (keys m))]
    (->> paths
         (map (partial path->distance m))
         (apply max))))