(ns earley
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.pldb :as pldb]
            [clojure.set :as set])
  (:use clojure.core.logic))

(defn- set-conj [{:keys [seen?] :as all} & xs]
  (let [xs (remove seen? xs)]
    (into {} (map (fn [[k v]] [k (apply conj v xs)])) all)))

(defn- next-symbol [{[_ rhs] :rule pos :pos}] (get rhs pos ::complete))

(defn- state->sppf [{{[lhs _] :rule :as lr0} :lr0 origin :origin :as st} k]
  {:label (if (= ::complete (next-symbol lr0)) lhs lr0) :start origin :finish k})

(def ^:private indexed?* (some-fn string? indexed?))

;; Returns a single parse tree.
(defn- single-parse [forest sppf]
  (letfn [(walk [sppf]
            (loop [sppf sppf lst ()]
              (if-let [[{{[lhs _] :rule pos :pos} :label :as left} right] (first (forest sppf))]
                (if (zero? pos)
                  [lhs (cons right lst)]
                  (recur left (cons right lst))))))
          (parse [sppf]
            (let [[rule symbs] (walk sppf)
                  symbs (map (some-fn :terminal parse) symbs)]
              (cons rule symbs)))]
    (parse sppf)))

;; Returns a lazy sequence of all possible parse trees.
;; I made it return all potential parses mainly for fun.
;; This uses core.logic so it's incredibly slow.
;; Only check all parses on small inputs!

(pldb/db-rel ^:private node ^:index sppf left right)

(defn- all-parses* [forest sppf]
  (let [forest (->> forest
                    (mapcat (fn [[k lst]] (map (partial apply vector node k) lst)))
                    (apply pldb/db))]
    (letfn [(mapo [rel lst out]
              (matche [lst out]
                ([[] []])
                ([[x . xs] [y . ys]] (rel x y) (mapo rel xs ys))))
            ;; TODO: Make this method less scuffed.
            (walko [sppf lst derivation]
              (fresh [pos lhs rhs left right]
                (node sppf left right)
                (featurec left {:label {:rule [lhs rhs] :pos pos}})
                (matchu [pos]
                  ([0] (conjo [lhs] (cons right lst) derivation))
                  ([_] (walko left (cons right lst) derivation)))))
            (parseo [sppf tree]
              (fresh [rule symbs out]
                (walko sppf () [rule symbs])
                (mapo #(conda [(featurec %1 {:terminal %2})] [(parseo %1 %2)]) symbs out)
                (conso rule out tree)))]
      (pldb/with-db forest (run* [q] (parseo sppf q))))))

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
                      {:chart [{:seen? (set top-levels) :items (vec top-levels)}] :forest {}}))
          parse (if all-parses all-parses* single-parse)]
      (-> chart
          (update :chart (partial map :items))
          (assoc :parse (parse (:forest chart) {:label S :start 0 :finish (count s)}))))))

(comment
  ;; Example on Wikipedia.
  (:parse (earley "2+3*4"
                  [[:P [:S]]
                   [:S [:S \+ :M]] [:S [:M]]
                   [:M [:M \* :T]] [:M [:T]]
                   [:T "1"] [:T "2"] [:T "3"] [:T "4"]]
                  :P))
  ;; Works with ambiguous grammars.
  (:parse (earley "sssssssssssssssss"
                  [[:S [:S :S]] [:S "s"]]
                  :S
                  :all-parses true))
  ;; Nullable grammar test.
  (earley "aa"
          [[:A [\a :B :B :B \a]]
           [:B ""]]
          :A))