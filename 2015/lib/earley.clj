(ns earley
  (:require [clojure.set :as set]))

;; TODO: Deal with nullable nonterminals.
(defn earley [s grm S]
  {:pre [((some-fn string? indexed?) s)
         (->> grm (map second) (every? (some-fn string? indexed?)))]}
  (letfn [(set-conj [{:keys [seen?] :as all} & xs]
            (let [xs (remove seen? xs)]
              (into {} (map (fn [[k v]] [k (apply conj v xs)])) all)))
          (next-symbol [{[_ rhs] :rule pos :pos}] (get rhs pos ::complete))
          (add-sppf [{{[lhs _] :rule :as lr0} :lr0 origin :origin :as st} k]
            (assoc st :sppf {:label (if (= ::complete (next-symbol lr0)) lhs lr0)
                             :start origin
                             :finish k}))
          (predict [chart {:keys [lr0]} k]
            (->> grm
                 (filter (comp #{(next-symbol lr0)} first))
                 (map #(hash-map :lr0 {:rule % :pos 0} :origin k))
                 (update chart k (partial apply set-conj))))
          (scan [chart {:keys [lr0] :as st} k]
            (cond-> chart
              (= (next-symbol lr0) (get s k ::end-of-string))
              (update (inc k)
                      (fnil set-conj {:seen? #{} :items []})
                      (-> st (update-in [:lr0 :pos] inc) (add-sppf (inc k))))))
          (complete [chart {{[lhs _] :rule} :lr0 origin :origin :as st} k]
            (->> (-> chart (get origin) :items)
                 (filter (comp #{lhs} next-symbol :lr0))
                 (map #(-> % (update-in [:lr0 :pos] inc) (add-sppf k)))
                 (update chart k (partial apply set-conj))))]
    (let [terminal? (set/difference (->> grm (mapcat second) set) (->> grm (map first) set))
          top-levels (->> grm
                          (filter (comp #{S} first))
                          (map #(hash-map :lr0 {:rule % :pos 0} :origin 0)))]
      (->> (range (inc (count s)))
           (reduce
            (fn [chart k]
              (loop [chart chart i 0]
                (let [items (:items (chart k))]
                  (if (= i (count items))
                    chart
                    (let [{:keys [lr0] :as st} (items i)]
                      (cond
                        (= ::complete (next-symbol lr0)) (recur (complete chart st k) (inc i))
                        (terminal? (next-symbol lr0)) (recur (scan chart st k) (inc i))
                        :else (recur (predict chart st k) (inc i))))))))
            [(zipmap [:seen? :items] ((juxt set vec) top-levels))])
           (map :items)))))

(comment
  ;; Example on Wikipedia.
  (earley "2+3*4"
          [[:P [:S]]
           [:S [:S \+ :M]] [:S [:M]]
           [:M [:M \* :T]] [:M [:T]]
           [:T "1"] [:T "2"] [:T "3"] [:T "4"]]
          :P)
  ;; Works with ambiguous grammars.
  (earley "sss"
          [[:S [:S :S]] [:S "s"]]
          :S))