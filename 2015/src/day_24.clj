(ns day-24
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(def input (slurp "input/d24.txt"))

(defn subset-sums-of-n [target n nums]
  (-> nums
      (comb/combinations n)
      (->> (filter #(= target (apply + %))))))

(defn p1 [input]
  (let [nums (->> input str/split-lines (map #(Integer/parseInt %)))
        goal (/ (apply + nums) 3)
        min-length-subsets (loop [i 1]
                             (if-let [xs (seq (subset-sums-of-n goal i nums))]
                               xs
                               (recur (inc i))))]
    (->> min-length-subsets
         (map (partial apply *))
         (apply min))))

(defn p2 [input]
  (let [nums (->> input str/split-lines (map #(Integer/parseInt %)))
        goal (/ (apply + nums) 4)
        min-length-subsets (loop [i 1]
                             (if-let [xs (seq (subset-sums-of-n goal i nums))]
                               xs
                               (recur (inc i))))]
    (->> min-length-subsets
         (map (partial apply *))
         (apply min))))

(comment
  (p1 input)
  (p2 input))
