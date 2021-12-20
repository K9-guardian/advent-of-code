(ns D13.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def input (slurp "input/d13.txt"))

(defn parse-line [l]
  (->>
   l
   (re-seq #"(.*) would (.*) (\d+) happiness units by sitting next to (.*).")
   nfirst))

(defn parsed->map-entry [[p1 signum amt p2]]
  (let [p1 (keyword p1)
        p2 (keyword p2)
        signum (case signum "gain" + "lose" -)
        amt (Integer/parseInt amt)]
    [p1 [p2 (signum amt)]]))

(defn arrangement->happiness [m a]
  (->>
   a
   (partition 3 1)
   (transduce
    (map
     (fn [[p1 p2 p3]]
       (+ (get-in m [p2 p1])
          (get-in m [p2 p3]))))
    +)))

(defn p1 [input]
  (as-> input $
    (str/split-lines $)
    (map (comp parsed->map-entry parse-line) $)
    (reduce
     (fn [m [k v]] (update m k (fnil conj {}) v))
     {}
     $)
    (let [arrangements (->>
                        $
                        keys
                        comb/permutations
                        (map #(take (-> % count (+ 2)) (cycle %))))]
      (->>
       arrangements
       (map (partial arrangement->happiness $))
       (apply max)))))

(defn p2 [input]
  (as-> input $
    (str/split-lines $)
    (map (comp parsed->map-entry parse-line) $)
    (reduce
     (fn [m [k v]] (update m k (fnil conj {:me 0}) v))
     {}
     $)
    (assoc $ :me (zipmap (keys $) (repeat 0)))
    (let [arrangements (->>
                        $
                        keys
                        comb/permutations
                        (map #(take (-> % count (+ 2)) (cycle %))))]
      (->>
       arrangements
       (map (partial arrangement->happiness $))
       (apply max)))))