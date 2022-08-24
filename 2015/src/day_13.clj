(ns day-13
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(def input (slurp "input/d13.txt"))

(defn parse-line [l]
  (let [[p1 signum amt p2] (->> l
                                (re-find #"(\w+) .* (gain|lose) (\d+) .* (\w+).")
                                rest)
        signum ({"gain" + "lose" -} signum)
        amt (Integer/parseInt amt)]
    {p1 {p2 (signum amt)}}))

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
               (map parse-line)
               (reduce (partial merge-with merge) {}))
        arrangements (->> m
                          keys
                          comb/permutations
                          (map #(take (+ 2 (count %)) (cycle %))))]
    (->> arrangements
         (map (partial arrangement->happiness m))
         (apply max))))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (reduce (partial merge-with (partial merge {"me" 0})) {}))
        m (assoc m "me" (zipmap (keys m) (repeat 0)))
        arrangements (->> m
                          keys
                          comb/permutations
                          (map #(take (+ 2 (count %)) (cycle %))))]
    (->> arrangements
         (map (partial arrangement->happiness m))
         (apply max))))
