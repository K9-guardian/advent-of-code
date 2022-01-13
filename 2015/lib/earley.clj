(ns earley
  (:require [clojure.set :as set]))

(defn- set-conj [{:keys [seen?] :as all} & xs]
  (let [xs (remove seen? xs)]
    (into {} (map (fn [[k v]] [k (apply conj v xs)])) all)))

(defn- next-symbol [{[_ rhs] :rule pos :pos}] (get rhs pos ::complete))

(defn- state->sppf [{{[lhs _] :rule :as lr0} :lr0 origin :origin :as st} k]
  {:label (if (= ::complete (next-symbol lr0)) lhs lr0) :start origin :finish k})

(defn- add-sppf [st k] (assoc st :sppf (state->sppf st k)))

(def ^:private indexed?* (some-fn string? indexed?))

;; TODO: Deal with nullable nonterminals.
;; TODO: Add derivation for complete.
(defn earley [s grm S]
  {:pre [(indexed?* s) (->> grm (map second) (every? indexed?*))]}
  (letfn [(predict [chart {:keys [lr0]} k]
            (->> grm
                 (filter (comp #{(next-symbol lr0)} first))
                 (map #(hash-map :lr0 {:rule % :pos 0} :origin k))
                 (update-in chart [:chart k] (partial apply set-conj))))
          (scan [chart {:keys [lr0] :as st} k]
            (if (= (next-symbol lr0) (get s k ::end-of-string))
              (let [prev (state->sppf st k)
                    symb {:terminal (next-symbol lr0)}
                    k (inc k)
                    st (update-in st [:lr0 :pos] inc)
                    sppf (state->sppf st k)]
                (-> chart
                    (update-in [:chart k]
                               (fnil set-conj {:seen? #{} :items []})
                               (assoc st :sppf (state->sppf st k)))
                    (update-in [:forest sppf] (fnil conj []) [prev symb])))
              chart))
          (complete [chart {{[lhs _] :rule} :lr0 origin :origin :as st} k]
            (->> (-> chart :chart (get origin) :items)
                 (filter (comp #{lhs} next-symbol :lr0))
                 (map #(-> % (update-in [:lr0 :pos] inc) (add-sppf k)))
                 (update-in chart [:chart k] (partial apply set-conj))))]
    (let [terminal? (set/difference (->> grm (mapcat second) set) (->> grm (map first) set))
          top-levels (->> grm
                          (filter (comp #{S} first))
                          (map #(hash-map :lr0 {:rule % :pos 0} :origin 0)))
          chart (->> (range (inc (count s)))
                     (reduce
                      (fn [chart k]
                        (loop [chart chart i 0]
                          (let [items (-> chart :chart (get k) :items)]
                            (if (= i (count items))
                              chart
                              (let [{:keys [lr0] :as st} (items i)]
                                (cond
                                  (= ::complete (next-symbol lr0)) (recur (complete chart st k)
                                                                          (inc i))
                                  (terminal? (next-symbol lr0)) (recur (scan chart st k) (inc i))
                                  :else (recur (predict chart st k) (inc i))))))))
                      {:chart [(zipmap [:seen? :items] ((juxt set vec) top-levels))] :forest {}}))]
      (update chart :chart (partial remove :seen?)))))

(comment
  ;; Example on Wikipedia.
  (:forest (earley "2+3*4"
                   [[:P [:S]]
                    [:S [:S \+ :M]] [:S [:M]]
                    [:M [:M \* :T]] [:M [:T]]
                    [:T "1"] [:T "2"] [:T "3"] [:T "4"]]
                   :P))
  ;; Works with ambiguous grammars.
  (earley "sss"
          [[:S [:S :S]] [:S "s"]]
          :S))