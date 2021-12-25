(ns D13.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(def input (slurp "input/d13.txt"))

(defn parse-line [l]
  (->> l
       (re-find #"(.*) would (.*) (\d+) happiness units by sitting next to (.*).")
       rest))

(defn parsed->map-entry [[p1 signum amt p2]]
  (let [p1 (keyword p1)
        p2 (keyword p2)
        signum (case signum "gain" + "lose" -)
        amt (Integer/parseInt amt)]
    [p1 [p2 (signum amt)]]))

(defn arrangement->happiness [m a]
  (->> a
       (partition 3 1)
       (transduce (map (fn [[p1 p2 p3]]
                         (+ (get-in m [p2 p1])
                            (get-in m [p2 p3]))))
                  +)))

(defn p1 [input]
  (let [m (->> input
               str/split-lines
               (map (comp parsed->map-entry parse-line))
               (reduce (fn [m [k v]] (update m k (fnil conj {}) v)) {}))
        arrangements (->> m
                          keys
                          comb/permutations
                          (map #(take (-> % count (+ 2)) (cycle %))))]
    (->> arrangements
         (map (partial arrangement->happiness m))
         (apply max))))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map (comp parsed->map-entry parse-line))
               (reduce (fn [m [k v]] (update m k (fnil conj {:me 0}) v)) {}))
        m* (assoc m :me (zipmap (keys m) (repeat 0)))
        arrangements (->> m*
                          keys
                          comb/permutations
                          (map #(take (-> % count (+ 2)) (cycle %))))]
    (->> arrangements
         (map (partial arrangement->happiness m*))
         (apply max))))