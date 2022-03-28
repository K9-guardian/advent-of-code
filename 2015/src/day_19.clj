(ns day-19
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str])
  (:use earley))

(def input (slurp "input/d19.txt"))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        rs (->> lines (drop-last 2) (map #(str/split % #" => ")))
        m (last lines)]
    [rs m]))

;; Like clojure.string/replace, but uses a collection of replacements
(defn string-replace* [s value rs]
  (loop [acc "" s s rs rs]
    (if-let [i (str/index-of s value)]
      (recur (str acc (subs s 0 i) (first rs))
             (subs s (+ i (count value)))
             (rest rs))
      (str acc s))))

(defn single-replacements [m [k v]]
  (->> m
       (re-seq (re-pattern k))
       rest
       (cons v)
       comb/permutations
       (map (partial string-replace* m k))))

(defn p1 [input]
  (let [[rs m] (parse-input input)]
    (-> rs
        (->> (mapcat (partial single-replacements m)))
        set
        (disj m)
        count)))

;; Technically our molecule is not a proper sentence as it has nonterminals.
;; However, we can make it a sentence by adding a rule A => a for every symbol,
;; then applying each of those rules to the molecule, thus creating a valid sentence.
(defn p2 [input]
  (let [[rs m] (parse-input input) ; Splitting by symbols
        rs (mapcat #(update % 1 (comp vec (partial re-seq #"[A-Z][a-z]?"))) rs)
        m (re-seq #"[A-Z][a-z]?" m)
        rs (->> m
                set ; Using all lowercase to represent terminals
                (mapcat (juxt identity (comp vector str/lower-case)))
                (concat rs))
        m (mapv str/lower-case m)]
    (->> (earley rs "e" m)
         :parse
         (tree-seq seq? identity)
         (remove seq?)
         count
         (+ (- (* 2 (count m))))))) ; Removes the extra rules we added for terminals