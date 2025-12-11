(ns day-17
  (:require [clojure.string :as str]))

(def input (slurp "input/d17.txt"))

;; clojure.math.combinatorics/subsets is too smart with handling duplicates
;; Using SICP instead
(defn subsets [l]
  (if (empty? l)
    [[]]
    (let [r (subsets (rest l))]
      (concat r (map (partial cons (first l)) r)))))

(defn p1 [input]
  (->> input
       str/split-lines
       (map #(Integer/parseInt %))
       subsets
       (filter #(= 150 (apply + %)))
       count))

(defn p2 [input]
  (->> input
       str/split-lines
       (map #(Integer/parseInt %))
       subsets
       (filter #(= 150 (apply + %)))
       (sort-by count <)
       (partition-by count)
       first
       count))

(comment
  (p1 input)
  (p2 input))
