(ns day-05
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

(def nice-p1?
  (every-pred contains-3-vowels?
              contains-consecutive-duplicate?
              (complement contains-banned-pair?)))

(defn contains-2-pairs-without-overlap? [s]
  (loop [b false
         [p & ps] (partition 2 1 s)]
    (case (count l)
      (1 2) b
      (recur (or b (some #{p} (nnext l)))
             ps))))

(defn contains-length-3-palindrome? [s]
  (some #(= (first %) (last %))
        (partition 3 1 s)))

(def nice-p2?
  (every-pred contains-2-pairs-without-overlap?
              contains-length-3-palindrome?))

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