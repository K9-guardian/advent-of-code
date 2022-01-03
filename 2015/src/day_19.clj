(ns day-19
  (:require [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.string :as str]))

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

;; Failed brute force.
(comment
  (defn min-steps [m rs]
    (def min-steps-rec
      (memoize
       (fn [m n]
         (if (= "e" m)
           n
           (let [replacements (->> rs
                                   (mapcat (partial replacements m))
                                   (remove #{m}))]
             (if (empty? replacements)
               ##Inf
               (->> replacements
                    (map #(min-steps-rec % (inc n)))
                    (apply min))))))))
    (min-steps-rec m 0)))

(defn earley [rs m s0 terminal?]
  (letfn [(set-conj [v & xs] (reduce (fn [v x] (if (some #{x} v) v (conj v x))) v xs))
          (finished? [st] (= (-> st :rule second count) (:pos st)))
          (next-symbol [st] (and (not (finished? st)) ((-> st :rule second) (:pos st))))
          (predicter [s st k]
            (let [B (next-symbol st)
                  predictions (filter (comp #{B} first) rs)]
              (->> predictions
                   (map #(hash-map :rule % :pos 0 :origin k))
                   (update s k (partial apply set-conj)))))
          (scanner [s st k]
            (let [a (next-symbol st)]
              (if (= a (get m k))
                (update s (inc k) conj (update st :pos inc))
                s)))
          (completer [s {[lhs _] :rule origin :origin} k]
            (let [completions (filter (comp #{lhs} next-symbol) (s origin))]
              (->> completions
                   (map #(update % :pos inc))
                   (update s k (partial apply set-conj)))))]
    (reduce
     (fn [s k]
       (loop [s s i 0]
         (if (= i (count (s k)))
           s
           (let [st ((s k) i)]
             (cond
               (finished? st) (recur (completer s st k) (inc i))
               (terminal? (next-symbol st)) (recur (scanner s st k) (inc i))
               :else (recur (predicter s st k) (inc i)))))))
     (let [top-levels (->> rs
                           (filter (comp #{s0} first))
                           (mapv #(hash-map :rule % :pos 0 :origin 0)))]
       (->> (repeat (count m) [])
            (cons top-levels)
            vec))
     (range (inc (count m))))))

(comment
  (let [m ["number" "+" "number" "*" "number"]
        rs [["P" ["S"]]
            ["S" ["S" "+" "M"]]
            ["S" ["M"]]
            ["M" ["M" "*" "T"]]
            ["M" ["T"]]
            ["T" ["number"]]]]
    (earley rs m "P" #{"number" "+" "*"})))

(comment
  (let [m ["h" "o" "h"]
        rs [["e" ["H"]]
            ["e" ["O"]]
            ["H" ["H" "O"]]
            ["H" ["O" "H"]]
            ["O" ["H" "H"]]
            ["H" ["h"]]
            ["O" ["o"]]]]
    (earley rs m "e" #{"h" "o"})))

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
  (let [[rs m] (parse-input input)
        rs (map (fn [[k v]] [k (re-seq #"[A-Z][a-z]?" v)]) rs) ; Splitting by symbols
        m (re-seq #"[A-Z][a-z]?" m)
        rs (->> m ; Using all lowercase to represent terminals
                (map (juxt identity (comp vector str/lower-case)))
                (concat rs))
        m (map str/lower-case m)
        terms (set/difference (->> rs (mapcat second) set)
                              (->> rs (map first) set))]
    (earley rs m "e" )))

(p2 input)