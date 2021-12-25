(ns D9.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(def input (slurp "input/d9.txt"))

(defn parse-line [l]
  (->> l
       (re-find #"(\w+) to (\w+) = (\d+)")
       rest))

(defn parsed->map-entries [[start end d]]
  (let [start (keyword start)
        end (keyword end)
        d (Integer/parseInt d)]
    [[start [end d]]
     [end [start d]]]))

(defn path->distance [m p]
  (reduce (fn [d [start end]]
            (+ d (get-in m [start end])))
          0
          (partition 2 1 p)))

(defn p1 [input]
  (let [m (->> input
               str/split-lines
               (map (comp parsed->map-entries parse-line))
               (reduce (fn [m [[k1 v1] [k2 v2]]]
                         (->
                          m
                          (update k1 (fnil conj {}) v1)
                          (update k2 (fnil conj {}) v2)))
                       {}))
        paths (-> m keys comb/permutations)]
    (->> paths
         (map (partial path->distance m))
         (apply min))))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map (comp parsed->map-entries parse-line))
               (reduce (fn [m [[k1 v1] [k2 v2]]]
                         (->
                          m
                          (update k1 (fnil conj {}) v1)
                          (update k2 (fnil conj {}) v2)))
                       {}))
        paths (-> m keys comb/permutations)]
    (->> paths
         (map (partial path->distance m))
         (apply max))))