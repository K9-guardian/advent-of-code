(ns day-12
  (:require [cheshire.core :as json]))

(def input (json/parse-string (slurp "input/d12.txt")))

(defn p1 [input]
  (->> input
       (tree-seq (some-fn vector? map?) identity)
       (filter number?)
       (apply +)))

(defn p2 [input]
  (->> input
       (tree-seq #(or (vector? %)
                      (and (map? %)
                           (->> % vals (some #{"red"}) nil?)))
                 identity)
       (filter number?)
       (apply +)))

(comment
  (p1 input)
  (p2 input))
