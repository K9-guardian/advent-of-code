(ns D12.core
  (:require [cheshire.core :refer :all]))

(def input (parse-string (slurp "input/d12.txt")))

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