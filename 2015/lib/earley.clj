(ns earley
  (:refer-clojure :exclude [==])
  (:require [clojure.set :as set])
  (:use clojure.core.logic))

(defn- set-conj [{:keys [seen?] :as all} & xs]
  (let [xs (remove seen? xs)]
    (into {} (map (fn [[k v]] [k (apply conj v xs)])) all)))

(defn- next-symbol [{[_ rhs] :rule pos :pos}] (get rhs pos ::complete))

(defn- state->sppf [{{[lhs _] :rule :as lr0} :lr0 origin :origin :as st} k]
  {:label (if (= ::complete (next-symbol lr0)) lhs lr0) :start origin :finish k})

(def ^:private indexed?* (some-fn string? indexed?))

;; Returns a single possible parse tree or a lazy sequence of all possible parse trees.
;; Note: I made it return all potential parses mainly for fun, this uses core.logic so
;;       it's incredibly slow, only use this on small sentences.
(defn- parse [forest sppf & {:keys [all-parses] :or {all-parses false}}]
  (letfn [(walk [sppf]
            (loop [sppf sppf lst ()]
              (if-let [[{{[lhs _] :rule pos :pos} :label :as left} right] (first (forest sppf))]
                (if (zero? pos)
                  [lhs (cons right lst)]
                  (recur left (cons right lst))))))
          (parse [sppf]
            (let [[rule symbs] (walk sppf)
                  symbs (map (some-fn :terminal parse) symbs)]
              (cons rule symbs)))
          (mapo [rel lst out]
            (conde
              [(emptyo lst) (== out ())]
              [(fresh [x xs y ys]
                 (conso x xs lst)
                 (conso y ys out)
                 (rel x y)
                 (mapo rel xs ys))]))
          (walko [sppf lst derivation]
            (fresh [pos lhs rhs left right]
              (project [sppf] (membero [left right] (forest sppf)))
              (featurec left {:label {:rule [lhs rhs] :pos pos}})
              (conde
                [(== 0 pos) (conjo [lhs] (cons right lst) derivation)]
                [(walko left (cons right lst) derivation)])))
          (parseo [sppf tree]
            (fresh [rule symbs out]
              (walko sppf () [rule symbs])
              (mapo (fn [x y]
                      (conde
                        [(featurec x {:terminal y})]
                        [(parseo x y)]))
                    symbs
                    out)
              (conso rule out tree)))]
    (if all-parses
      (run* [q] (parseo sppf q))
      (parse sppf))))

;; TODO: Deal with nullable nonterminals.
(defn earley [s grm S & {:keys [all-parses] :or {all-parses false}}]
  {:pre [(indexed?* s) (->> grm (map second) (every? indexed?*))]}
  (letfn [(predict [chart {:keys [lr0]} k]
            (->> grm
                 (filter (comp #{(next-symbol lr0)} first))
                 (map #(hash-map :lr0 {:rule % :pos 0} :origin k))
                 (update-in chart [:chart k] (partial apply set-conj))))
          (scan [chart {:keys [lr0] :as st} k]
            (if (= (next-symbol lr0) (get s k ::end-of-string))
              (let [prev (state->sppf st k)
                    symb {:terminal (next-symbol lr0) :start k :finish (inc k)}
                    st (update-in st [:lr0 :pos] inc)
                    sppf (state->sppf st (inc k))]
                (-> chart
                    (update-in [:chart (inc k)] (fnil set-conj {:seen? #{} :items []}) st)
                    (update-in [:forest sppf] (fnil conj []) [prev symb])))
              chart))
          (complete [chart {{[lhs _] :rule} :lr0 origin :origin :as st} k]
            (let [completions (filter (comp #{lhs} next-symbol :lr0)
                                      (-> chart :chart (get origin) :items))]
              (reduce
               (fn [chart {:keys [lr0] :as st}]
                 (let [prev (state->sppf st origin)
                       symb {:label (next-symbol lr0) :start origin :finish k}
                       st (update-in st [:lr0 :pos] inc)
                       sppf (state->sppf st k)]
                   (-> chart
                       (update-in [:chart k] set-conj st)
                       (update-in [:forest sppf] (fnil conj []) [prev symb]))))
               chart
               completions)))]
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
                                (condp apply [(next-symbol lr0)] ; Unary predicates with condp.
                                  #{::complete} (recur (complete chart st k) (inc i))
                                  terminal? (recur (scan chart st k) (inc i))
                                  (recur (predict chart st k) (inc i))))))))
                      {:chart [{:seen? (set top-levels) :items (vec top-levels)}] :forest {}}))]
      (-> chart
          (update :chart (partial map :items))
          (assoc :parse
                 (parse (:forest chart)
                        {:label S :start 0 :finish (count s)}
                        :all-parses all-parses))))))

(comment
  ;; Example on Wikipedia.
  (:parse (earley "2+3*4"
                  [[:P [:S]]
                   [:S [:S \+ :M]] [:S [:M]]
                   [:M [:M \* :T]] [:M [:T]]
                   [:T "1"] [:T "2"] [:T "3"] [:T "4"]]
                  :P))
  ;; Works with ambiguous grammars.
  (:parse (earley "sssss"
                  [[:S [:S :S]] [:S "s"]]
                  :S
                  :all-parses true))
  ;; Nullable grammar test.
  (earley "aa"
          [[:A [\a :B :B :B \a]]
           [:B ""]]
          :A))