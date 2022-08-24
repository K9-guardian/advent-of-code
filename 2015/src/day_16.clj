(ns day-16
  (:require [clojure.string :as str]))

(def input (slurp "input/d16.txt"))

(def scan
  {"children" 3
   "cats" 7
   "samoyeds" 2
   "pomeranians" 3
   "akitas" 0
   "vizslas" 0
   "goldfish" 5
   "trees" 3
   "cars" 2
   "perfumes" 1})

(defn parse-line [l]
  (let [[_ & vs] (str/split l #": |, ")
        vs (->> vs
                (partition 2)
                (map (fn [[k v]] [k (Integer/parseInt v)]))
                (into {}))]
    vs))

(defn maybe-aunt-p1? [scan]
  (fn [m]
    (every? (fn [[k v]] (= v (scan k))) m)))

(defn maybe-aunt-p2? [scan]
  (fn [m]
    (every? (fn [[k v]]
              (case k
                ("cats" "trees") (> v (scan k))
                ("pomeranians" "goldfish") (< v (scan k))
                (= v (scan k))))
            m)))

(defn p1 [input]
  (let [parsed (->> input str/split-lines (map parse-line))]
    (-> parsed
        (->> (map (maybe-aunt-p1? scan)))
        (.indexOf true)
        inc)))

(defn p2 [input]
  (let [parsed (->> input str/split-lines (map parse-line))]
    (-> parsed
        (->> (map (maybe-aunt-p2? scan)))
        (.indexOf true)
        inc)))
