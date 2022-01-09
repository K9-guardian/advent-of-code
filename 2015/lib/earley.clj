(ns earley
  (:require [clojure.set :as set]))

(defn earley [s grm S & {:keys [parse-tree?] :or {parse-tree? false}}]
  (letfn [(set-conj [{:keys [seen?] :as all} & xs]
            (let [xs (remove seen? xs)]
              (into {} (map (fn [[k v]] [k (apply conj v xs)])) all)))
          (finished? [st] (= (-> st :rule second count) (:pos st)))
          (next-symbol [st] (and (not (finished? st)) ((-> st :rule second) (:pos st))))
          (predict [chart st k]
            (let [predictions (filter (comp #{(next-symbol st)} first) grm)]
              (->> predictions
                   (map #(hash-map :rule % :pos 0 :origin k))
                   (update chart k (partial apply set-conj)))))
          (scan [chart st k]
            (cond-> chart
              (= (next-symbol st) (get s k))
              (update (inc k) (fnil set-conj {:seen? #{} :items []}) (update st :pos inc))))
          (complete [chart {[lhs _] :rule origin :origin} k]
            (let [completions (filter (comp #{lhs} next-symbol) (-> chart (get origin) :items))]
              (->> completions
                   (map #(update % :pos inc))
                   (update chart k (partial apply set-conj)))))
          (parse-tree [chart]
            (let [chart (->> chart
                             (mapcat (fn [i s]
                                       (->> s
                                            (filter finished?)
                                            (map #(-> % (dissoc :pos) (assoc :end i)))))
                                     (range))
                             (group-by :origin)
                             (into {}))
                  dfs (fn dfs [{:keys [origin end] [_ rhs] :rule}]
                        (reduce (fn [{[i j] :nodes tree :tree} symb]
                                  (let [pred (if (= j end) = >)]
                                    ))
                                {:nodes [origin end] :tree []}
                                rhs))]
              (dfs (-> chart
                       (get 0)
                       (->> (filter (comp #{(count s)} :end)))
                       first))))]
    (let [terminal? (set/difference (->> grm (mapcat second) set) (->> grm (map first) set))
          top-levels (->> grm
                          (filter (comp #{S} first))
                          (map #(hash-map :rule % :pos 0 :origin 0)))
          chart (->> (range (inc (count s)))
                     (reduce
                      (fn [chart k]
                        (loop [chart chart i 0]
                          (let [items (:items (chart k))]
                            (if (= i (count items))
                              chart
                              (let [st (items i)]
                                (cond
                                  (finished? st) (recur (complete chart st k) (inc i))
                                  (terminal? (next-symbol st)) (recur (scan chart st k) (inc i))
                                  :else (recur (predict chart st k) (inc i))))))))
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
          :P
          :parse-tree? true)
  (earley ["s" "s" "s"]
          [[:S [:S :S]]
           [:S ["s"]]]
          :S
          :parse-tree? true))