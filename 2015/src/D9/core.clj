(ns D9.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def input (slurp "input/d9.txt"))

(defn parse-string [s]
  (->>
   s
   (re-seq #"(\w+) to (\w+) = (\d+)")
   nfirst))

(defn path->distance [m p]
  (reduce
   (fn [d [start end]]
     (+ d (get-in m [start end])))
   0
   (partition 2 1 p)))

(defn p1 [input]
  (as-> input $
    (str/split-lines $)
    (map parse-string $)
    (reduce
     (fn [m [start end d]]
       (->
        m
        (update (keyword start) (fnil conj {}) [(keyword end) (Integer/parseInt d)])
        (update (keyword end) (fnil conj {}) [(keyword start) (Integer/parseInt d)])))
     {}
     $)
    (let [paths (-> $ keys comb/permutations)]
      (->>
       paths
       (map (partial path->distance $))
       (apply min)))))

(defn p2 [input]
  (as-> input $
    (str/split-lines $)
    (map parse-string $)
    (reduce
     (fn [m [start end d]]
       (->
        m
        (update (keyword start) (fnil conj {}) [(keyword end) (Integer/parseInt d)])
        (update (keyword end) (fnil conj {}) [(keyword start) (Integer/parseInt d)])))
     {}
     $)
    (let [paths (-> $ keys comb/permutations)]
      (->>
       paths
       (map (partial path->distance $))
       (apply max)))))