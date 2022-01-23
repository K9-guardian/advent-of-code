(ns earley
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.pldb :as pldb]
            [clojure.set :as set])
  (:use clojure.core.logic))

(declare nullables single-parse all-parses)

(defn- set-conj [{:keys [seen?] :as all} & xs]
  (let [xs (remove seen? xs)]
    (into {} (map (fn [[k v]] [k (into v xs)])) all)))

(defn- next-symbol [{[_ rhs] :rule pos :pos}] (get rhs pos ::complete))

(defn- state->label [{{[lhs _] :rule :as lr0} :lr0 origin :origin :as st} k]
  {:label (if (= ::complete (next-symbol lr0)) lhs lr0) :start origin :finish k})

(def ^:private indexed?* (some-fn string? indexed?))

(defn earley [s grm S & {all-parses* :all-parses :or {all-parses false}}]
  {:pre [(even? (count grm)) (indexed?* s) (->> grm (partition 2) (map second) (every? indexed?*))]}
  (let [grm (->> grm (partition 2) (map vec))
        nullable? (nullables grm)
        terminal? (set/difference (->> grm (mapcat second) set) (->> grm (map first) set))
        top-levels (->> grm
                        (filter (comp #{S} first))
                        (map #(hash-map :lr0 {:rule % :pos 0} :origin 0)))
        parse (if all-parses* all-parses single-parse)
        predict (fn [chart {:keys [lr0] :as st} k]
                  (let [predictions (->> grm
                                         (filter (comp #{(next-symbol lr0)} first))
                                         (map #(hash-map :lr0 {:rule % :pos 0} :origin k)))]
                    (reduce (fn [chart {{[lhs _] :rule} :lr0 :as pred}]
                              (if (nullable? lhs)
                                (let [prev (state->label st k)
                                      symb {:label (next-symbol lr0) :start k :finish k}
                                      st (update-in st [:lr0 :pos] inc)
                                      label (state->label st k)]
                                  (-> chart
                                      (update-in [:chart k] set-conj st pred)
                                      (update-in [:forest label] (fnil conj #{}) [prev symb])
                                      (update-in [:forest symb]
                                                 (fnil conj #{})
                                                 [{:label {:rule [(next-symbol lr0) []] :pos 0}
                                                   :start k
                                                   :finish k}
                                                  {:terminal [] :start k :finish k}])))
                                (update-in chart [:chart k] set-conj pred)))
                            chart
                            predictions)))
        scan (fn [chart {:keys [lr0] :as st} k]
               (if (= (next-symbol lr0) (get s k ::end-of-string))
                 (let [prev (state->label st k)
                       symb {:terminal (next-symbol lr0) :start k :finish (inc k)}
                       st (update-in st [:lr0 :pos] inc)
                       label (state->label st (inc k))]
                   (-> chart
                       (update-in [:chart (inc k)] (fnil set-conj {:seen? #{} :items []}) st)
                       (update-in [:forest label] (fnil conj #{}) [prev symb])))
                 chart))
        complete (fn [chart {{[lhs _] :rule} :lr0 origin :origin :as st} k]
                   (let [completions (filter (comp #{lhs} next-symbol :lr0)
                                             (-> chart :chart (get origin) :items))]
                     (reduce
                      (fn [chart {:keys [lr0] :as st}]
                        (let [prev (state->label st origin)
                              symb {:label (next-symbol lr0) :start origin :finish k}
                              st (update-in st [:lr0 :pos] inc)
                              label (state->label st k)]
                          (-> chart
                              (update-in [:chart k] set-conj st)
                              (update-in [:forest label] (fnil conj #{}) [prev symb]))))
                      chart
                      completions)))
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
        (assoc :parse (parse (:forest chart) {:label S :start 0 :finish (count s)})))))

;; Returns all the nullable nonterminals in a grammar.
(defn- nullables [grm]
  (loop [prev #{}
         curr (->> grm
                   (filter (comp empty? second))
                   (map first)
                   set)]
    (if (= prev curr)
      curr
      (recur curr
             (->> grm
                  (filter (comp (partial every? curr) second))
                  (map first)
                  (into curr))))))

;; Returns a single parse tree.
(defn- single-parse [forest label]
  (letfn [(walk [label]
            (loop [label label lst ()]
              (if-let [[{{[lhs _] :rule pos :pos} :label :as left} right] (first (forest label))]
                (if (zero? pos)
                  [lhs (cons right lst)]
                  (recur left (cons right lst))))))
          (parse [label]
            (let [[rule symbs] (walk label)]
              (->> symbs
                   (map #(if (contains? % :terminal) (:terminal %) (parse %)))
                   (cons rule))))]
    (parse label)))

(pldb/db-rel ^:private node ^:index label left right)

;; Returns a lazy sequence of all possible parse trees.
;; I made it return all potential parses mainly for fun.
;; This uses core.logic so it's incredibly slow.
;; Only check all parses on small inputs!
(defn- all-parses [forest label]
  (let [forest (->> forest
                    (mapcat (fn [[k sppf]] (map (partial apply list node k) sppf)))
                    (apply pldb/db))
        mapo (fn mapo [rel lst out]
               (matche [lst out]
                 ([[] []])
                 ([[x . xs] [y . ys]]
                  (rel x y)
                  (mapo rel xs ys))))
        walko (fn walko
                ([label out] (walko label () out))
                ([label acc out]
                 (fresh [pos lhs rhs left right acc*]
                   (node label left right)
                   (featurec left {:label {:rule [lhs rhs] :pos pos}})
                   (conso right acc acc*)
                   (matchu [pos]
                     ([0] (conjo [lhs] acc* out))
                     ([_] (walko left acc* out))))))
        parseo (fn parseo [label tree]
                 (fresh [rule symbs out]
                   (walko label [rule symbs])
                   (conso rule out tree)
                   (mapo #(conda
                            [(featurec %1 {:terminal %2})]
                            [(parseo %1 %2)])
                         symbs
                         out)))]
    (pldb/with-db forest (run* [q] (parseo label q)))))

(comment
  ;; Example on Wikipedia.
  (:parse (earley "2+3*4"
                  [:P [:S]
                   :S [:S \+ :M] :S [:M]
                   :M [:M \* :T] :M [:T]
                   :T "1" :T "2" :T "3" :T "4"]
                  :P))
  ;; Works with ambiguous grammars.
  (:parse (earley "sssss"
                  [:S [:S :S] :S "s"]
                  :S
                  :all-parses true))
  ;; Nullable grammar test. Skips production of C in parse tree.
  (:parse (earley "aa"
                  [:A [\a :B :B :B \a]
                   :B [:C]
                   :C ""]
                  :A)))