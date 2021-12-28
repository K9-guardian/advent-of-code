(ns D5.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d5.txt"))

(defn contains-3-vowels? [s]
  (->> s
       (filter #{\a \e \i \o \u})
       count
       (<= 3)))

(defn contains-consecutive-duplicate? [s]
  (< (count (dedupe s))
     (count s)))

(defn contains-banned-pair? [s]
  (some #{[\a \b] [\c \d] [\p \q] [\x \y]}
        (partition 2 1 s)))

(defn nice-p1? [s]
  ((every-pred contains-3-vowels?
               contains-consecutive-duplicate?
               (complement contains-banned-pair?))
   s))

(defn contains-2-pairs-without-overlap? [s]
  (loop [b false
         l (partition 2 1 s)]
    (case (count l)
      (1 2) b
      (recur (or b (some #{(first l)} (nnext l)))
             (rest l)))))

(defn contains-length-3-palindrome? [s]
  (some #(= (first %) (last %))
        (partition 3 1 s)))

(defn nice-p2? [s]
  ((every-pred contains-2-pairs-without-overlap?
               contains-length-3-palindrome?)
   s))

(defn p1 [input]
  (->> input
       str/split-lines
       (filter nice-p1?)
       count))

(defn p2 [input]
  (->> input
       str/split-lines
       (filter nice-p2?)
       count))