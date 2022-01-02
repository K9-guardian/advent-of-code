(ns day-19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def input (slurp "input/d19.txt"))

(def inf ##Inf)

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
#_(defn min-steps [m rs]
    (def min-steps-rec
      (memoize
       (fn [m n]
         (if (= "e" m)
           n
           (let [replacements (->> rs
                                   (mapcat (partial replacements m))
                                   (remove #{m}))]
             (if (empty? replacements)
               inf
               (->> replacements
                    (map #(min-steps-rec % (inc n)))
                    (apply min))))))))
    (min-steps-rec m 0))

#_(defn earley [m rs]
    (let [s (vec (repeat (inc (count m)) (sorted-set)))]
      (swap! s update 0 conj)))

;; function EARLEY_PARSE(words, grammar)
;;     INIT(words)
;;     ADD_TO_SET((γ → •S, 0), S[0])
;;     for k ← from 0 to LENGTH(words) do
;;         for each state in S[k] do  // S[k] can expand during this loop
;;             if not FINISHED(state) then
;;                 if NEXT_ELEMENT_OF(state) is a nonterminal then
;;                     PREDICTOR(state, k, grammar)         // non_terminal
;;                 else do
;;                     SCANNER(state, k, words)             // terminal
;;             else do
;;                 COMPLETER(state, k)
;;         end
;;     end
;;     return chart

(defn p1 [input]
  (let [[rs m] (parse-input input)]
    (-> rs
        #_first
        #_(->> (single-replacements m))
        (->> (mapcat (partial single-replacements m)))
        set
        (disj m)
        count)))

(p1 input)

(defn p2 [input]
  (let [[rs m] (parse-input input)
        m (re-seq #"[A-Z][a-z]?|e" m)
        rs (reduce (fn [m [k vs]] (merge m (zipmap vs (repeat k)))) {} rs)]
    (min-steps m rs)))

(let [m ["H" "O" "H" "O" "H" "O"]
      rs {"H" "e"
          "O" "e"
          "HO" "H"
          "OH" "H"
          "HH" "O"}]
  (min-steps m rs))

(p2 input)