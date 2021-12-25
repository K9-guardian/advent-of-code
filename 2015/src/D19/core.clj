(ns D19.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def input (slurp "input/d19.txt"))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        rs (drop-last 2 lines)
        m (last lines)]
    [(reduce (fn [m [k v]] (update m k (fnil conj []) v))
             {}
             (map #(str/split % #" => ") rs))
     m]))

;; Like clojure.string/replace, but uses a collection of replacements
(defn string-replace* [s value rs]
  (loop [acc "" s s [r & more] rs]
    (if-let [i (str/index-of s value)]
      (recur (str acc (subs s 0 i) r)
             (subs s (+ i (count value)))
             more)
      (str acc s))))

(defn replacements [m [k vs]]
  (let [matches (re-seq (re-pattern k) m)]
    (->> (cons vs (rest (map list matches)))
         comb/permutations
         (mapcat (partial apply comb/cartesian-product))
         (map (partial string-replace* m k)))))

(defn p1 [input]
  (let [[rs m] (parse-input input)]
    (-> rs
        (->> (mapcat (partial replacements m)))
        set
        (disj m)
        count)))

;; Implement this later when I'm stronger
(declare min-steps)

(defn p2 [input]
  (let [[rs m] (parse-input input)
        rs-rev (reduce (fn [m [k vs]] (merge m (zipmap vs (repeat [k]))))
                       {}
                       rs)]
    (min-steps rs-rev "e" m)))