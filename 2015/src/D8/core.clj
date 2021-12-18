(ns D8.core
  (:require [clojure.string :as str]))

(def input (slurp "d8.txt"))

(defn string->chars-in-memory [s]
  (count (re-seq #"\\\\|\\\"|\\x[\da-f]{2}|[^\"]" s)))

(defn string->chars-for-encoding [s]
  (reduce
   #(+ %1
       (case %2
         (\\ \") 2
         1))
   2
   s))

(defn p1 [input]
  (->>
   input
   str/split-lines
   (map #(- (count %) (-> % string->chars-in-memory)))
   (reduce +)))

(defn p2 [input]
  (->>
   input
   str/split-lines
   (map #(- (-> % string->chars-for-encoding) (count %)))
   (reduce +)))