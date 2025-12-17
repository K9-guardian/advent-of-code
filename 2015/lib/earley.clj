(ns earley
  (:refer-clojure :exclude [==])
  (:require [clojure.set :as set])
  (:use clojure.core.logic))

(declare nullables single-parse all-parses)

;; An Earley set is a tuple of a vector and a set.
;; This lets us iterate over them as well as keep track of seen values.
(defn- set-conj [{:keys [seen?] :as all} & xs]
  (let [xs (remove seen? xs)]
    (into {} (map (fn [[k v]] [k (into v xs)])) all)))

;; LR(0) items in the literature are a tuple of a grammar rule, matched position, and  origin.
;; In this implementation, the tuple (rule, pos) is referred to as lr0.
;; The tuple ((rule, pos), origin) is referred to as item.
(defn- next-symbol [{[_ rhs] :rule pos :pos}] (get rhs pos ::complete))

;; Labels are used for the shared pack parse forest.
;; They indicate the rule and it's match, and the section of corresponding input.
;; This function takes an item and creates a label using its origin and an inputted finish.
(defn- item->label [{{[lhs _] :rule :as lr0} :lr0 origin :origin :as item} k]
  {:label (if (= ::complete (next-symbol lr0)) lhs lr0) :start origin :finish k})

;; Strings aren't indexed? even though you can call get on them.
(def ^:private indexed?* (some-fn string? indexed?))

;; https://en.wikipedia.org/wiki/Earley_parser#The_algorithm
;; https://en.wikipedia.org/wiki/Earley_parser#Constructing_the_parse_forest
;; https://courses.engr.illinois.edu/cs421/sp2012/project/PracticalEarleyParsing.pdf
(defn earley [grm S s & {all-parses :all-parses :or {all-parses false}}]
  {:pre [(even? (count grm)) (indexed?* s) (->> grm (partition 2) (map second) (every? indexed?*))]}
  (let [grm (->> grm (partition 2) (map vec))
        nullable? (nullables grm)
        terminal? (set/difference (->> grm (mapcat second) set) (->> grm (map first) set))
        top-levels (->> grm
                        (filter (comp #{S} first))
                        (map #(hash-map :lr0 {:rule % :pos 0} :origin 0)))
        parse (if all-parses earley/all-parses single-parse) ; Namespace qualify to grab function

        predict (fn [state {:keys [lr0] :as input-item} k]
                  (let [predictions (->> grm
                                         (filter (comp #{(next-symbol lr0)} first))
                                         (map #(hash-map :lr0 {:rule % :pos 0} :origin k)))]
                    (reduce (fn [state {{[lhs _] :rule} :lr0 :as item}]
                              (if (nullable? lhs) ; Short circuit nullable productions.
                                (let [item-label (item->label input-item k)
                                      next-label {:label (next-symbol lr0) :start k :finish k}
                                      input-item* (update-in input-item [:lr0 :pos] inc)
                                      input-item*-label (item->label input-item* k)]
                                  (-> state
                                      (update-in [:chart k] set-conj input-item* item)
                                      (update-in [:forest input-item*-label]
                                                 (fnil conj #{})
                                                 [item-label next-label])
                                      (update-in [:forest next-label]
                                                 (fnil conj #{})
                                                 [{:label {:rule [(next-symbol lr0) []] :pos 0}
                                                   :start k
                                                   :finish k}
                                                  {:terminal [] :start k :finish k}])))
                                (update-in state [:chart k] set-conj item)))
                            state
                            predictions)))

        scan (fn [state {:keys [lr0] :as item} k]
               (if (= (next-symbol lr0) (get s k ::eos))
                 (let [item-label (item->label item k)
                       next-label {:terminal (next-symbol lr0) :start k :finish (inc k)}
                       item* (update-in item [:lr0 :pos] inc)
                       item*-label (item->label item* (inc k))]
                   (-> state
                       (update-in [:chart (inc k)] (fnil set-conj {:seen? #{} :items []}) item*)
                       (update-in [:forest item*-label] (fnil conj #{}) [item-label next-label])))
                 state))

        complete (fn [state {{[lhs _] :rule} :lr0 origin :origin} k]
                   (let [completions (filter (comp #{lhs} next-symbol :lr0)
                                             (-> state :chart (get origin) :items))]
                     (reduce
                      (fn [state {:keys [lr0] :as item}]
                        ;; Note: this origin is bound to the one in the input item.
                        (let [item-label (item->label item origin)
                              next-label {:label (next-symbol lr0) :start origin :finish k}
                              item* (update-in item [:lr0 :pos] inc)
                              item*-label (item->label item* k)]
                          (-> state
                              (update-in [:chart k] set-conj item*)
                              (update-in [:forest item*-label]
                                         (fnil conj #{})
                                         [item-label next-label]))))
                      state
                      completions)))

        ;; The state is a tuple of the earley chart and the shared pack parse forest.
        state (->> (range (inc (count s)))
                   (reduce
                    (fn [state k]
                      (loop [state state i 0]
                        (let [items (-> state :chart (get k) :items)]
                          (if (= i (count items))
                            state
                            (let [{:keys [lr0] :as st} (items i)]
                              (condp apply [(next-symbol lr0)] ; Unary predicates with condp.
                                #{::complete} (recur (complete state st k) (inc i))
                                terminal? (recur (scan state st k) (inc i))
                                (recur (predict state st k) (inc i))))))))
                    {:chart (-> (count s)
                                (repeat {:seen? #{} :items []})
                                (conj {:seen? (set top-levels) :items (vec top-levels)})
                                vec)
                     :forest {}}))]
    (-> state
        (update :chart (partial map :items)) ; We don't need the duplicate items from the set
        (assoc :parse (parse (:forest state) {:label S :start 0 :finish (count s)})))))

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

;; Returns a single parse tree by walking down the left tree of the forest.
(defn- single-parse [forest label]
  (letfn [(walk [label]
            (loop [label label lst ()]
              (when-let [[{{[lhs _] :rule pos :pos} :label :as left} right] (first (forest label))]
                (if (zero? pos)
                  [lhs (cons right lst)]
                  (recur left (cons right lst))))))
          (parse [label]
            (let [[rule symbs] (walk label)]
              (->> symbs
                   (map #(if (contains? % :terminal) (:terminal %) (parse %)))
                   (cons rule))))]
    (parse label)))

;; Returns a lazy sequence of all possible parse trees.
#_:clj-kondo/ignore
(defn- all-parses [forest label]
  (let [mapo (fn mapo [rel lst out]
               (matche [lst out]
                 ([[] []])
                 ([[x . xs] [y . ys]]
                  (rel x y)
                  (mapo rel xs ys))))
        walko (fn walko
                ([label out] (walko label () out))
                ([label acc out]
                 (fresh [pos lhs rhs left right]
                   (project [label] (membero [left right] (-> forest (get label) seq)))
                   (featurec left {:label {:rule [lhs rhs] :pos pos}})
                   (matchu [pos]
                     ([0] (conjo [lhs] (cons right acc) out))
                     ([_] (walko left (cons right acc) out))))))
        parseo (fn parseo [label tree]
                 (fresh [rule symbs out]
                   (walko label [rule symbs])
                   (conso rule out tree)
                   (mapo #(conda
                            [(featurec %1 {:terminal %2})]
                            [(parseo %1 %2)])
                         symbs
                         out)))]
    (run* [q] (parseo label q))))

(comment
  ;; Example on Wikipedia.
  (:parse (earley [:P [:S]
                   :S [:S \+ :M] :S [:M]
                   :M [:M \* :T] :M [:T]
                   :T "1" :T "2" :T "3" :T "4"]
                  :P
                  "2+3*4"))
  ;; Works with ambiguous grammars.
  (:parse (earley [:S [:S :S] :S "s"]
                  :S
                  "sssss"
                  :all-parses true))
  ;; Nullable grammar test. Skips production of C in parse tree.
  (:parse (earley [:A [\a :B :B :B \a]
                   :B [:C]
                   :C ""]
                  :A
                  "aa")))
