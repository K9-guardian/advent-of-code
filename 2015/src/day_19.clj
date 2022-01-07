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

(defn earley [m rs S & {:keys [parse-tree?] :or {parse-tree? false}}]
  (letfn [(set-conj [{:keys [seen?] :as all} & xs]
            (let [xs (remove seen? xs)]
              (into {} (map (fn [[k v]] [k (apply conj v xs)])) all)))
          (finished? [st] (= (-> st :rule second count) (:pos st)))
          (next-symbol [st] (and (not (finished? st)) ((-> st :rule second) (:pos st))))
          (predict [s st k]
            (let [predictions (filter (comp #{(next-symbol st)} first) rs)]
              (->> predictions
                   (map #(hash-map :rule % :pos 0 :origin k))
                   (update s k (partial apply set-conj)))))
          (scan [s st k]
            (cond-> s
              (= (next-symbol st) (get m k))
              (update (inc k) (fnil set-conj {:seen? #{} :items []}) (update st :pos inc))))
          (complete [s {[lhs _] :rule origin :origin} k]
            (let [completions (filter (comp #{lhs} next-symbol) (-> s (get origin) :items))]
              (->> completions
                   (map #(update % :pos inc))
                   (update s k (partial apply set-conj)))))
          (parse-tree [chart]
            (->> chart
                 (mapcat
                  (fn [i s]
                    (->> s
                         (filter finished?)
                         (map #(-> %
                                   (dissoc :pos)
                                   (assoc :end i)))))
                  (range))
                 (group-by :origin)
                 (into {} (map (fn [[k v]] [k (map #(dissoc % :origin) v)])))))]
    (let [terminal? (set/difference (->> rs (mapcat second) set) (->> rs (map first) set))
          top-levels (->> rs
                          (filter (comp #{S} first))
                          (map #(hash-map :rule % :pos 0 :origin 0)))
          chart (->> (range (inc (count m)))
                     (reduce
                      (fn [s k]
                        (loop [s s i 0]
                          (let [items (:items (s k))]
                            (if (= i (count items))
                              s
                              (let [st (items i)]
                                (cond
                                  (finished? st) (recur (complete s st k) (inc i))
                                  (terminal? (next-symbol st)) (recur (scan s st k) (inc i))
                                  :else (recur (predict s st k) (inc i))))))))
                      [(zipmap [:seen? :items] ((juxt set vec) top-levels))])
                     (map :items))]
      (cond-> chart
        parse-tree? parse-tree))))

(comment
  (earley ["num" "+" "num" "*" "num"]
          [[:P [:S]]
           [:S [:S "+" :M]]
           [:S [:M]]
           [:M [:M "*" :T]]
           [:M [:T]]
           [:T ["num"]]]
          :P)
  (earley ["h" "o" "h"]
          [[:e [:H]]
           [:e [:O]]
           [:H [:H :O]]
           [:H [:O :H]]
           [:O [:H :H]]
           [:H ["h"]]
           [:O ["o"]]]
          :e)
  (earley ["s" "s" "s"]
          [[:S [:S :S]]
           [:S ["s"]]]
          :S
          :parse-tree? true))

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
        rs (map (fn [[k v]] [k (vec (re-seq #"[A-Z][a-z]?" v))]) rs) ; Splitting by symbols
        m (re-seq #"[A-Z][a-z]?" m)
        rs (->> m
                set ; Using all lowercase to represent terminals
                (map (juxt identity (comp vector str/lower-case)))
                (concat rs))
        m (mapv str/lower-case m)]
    (->> (earley m rs "e")
         parse-tree
         (tree-seq vector? identity)
         count)))

(p2 input)