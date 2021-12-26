(ns D8.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d8.txt"))

(defn string->num-chars-in-memory [s]
  (->> s
       (re-seq #"\\\\|\\\"|\\x[\da-f]{2}|[^\"]")
       count))

(defn string->num-chars-for-encoding [s]
  (reduce #(+ %1
              (case %2
                (\\ \") 2
                1))
          2
          s))

(defn p1 [input]
  (->> input
       str/split-lines
       (map #(- (count %) (string->num-chars-in-memory %)))
       (reduce +)))

(defn p2 [input]
  (->> input
       str/split-lines
       (map #(- (string->num-chars-for-encoding %) (count %)))
       (reduce +)))