(ns D22.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d22.txt"))

(def parse-key
  {"Hit Points" :hp
   "Damage" :dmg})

(defn parse-input [input]
  (->> input
       str/split-lines
       (map (comp (fn [[k v]] [(parse-key k) (Integer/parseInt v)])
                  #(str/split % #": ")))
       (into {:arm 0})))

;; Like find but with a key fn
(defn find-key [k s l]
  (when-first [x l]
    (if (= s (k x))
      x
      (recur k s (rest l)))))

(defn damage [st person amt]
  (let [amt (max 1 (- amt (-> st person :arm)))]
    (update-in st [person :hp] - amt)))

(def spells
  [{:name "Magic Missile"
    :cost 53
    :action #(damage % :boss 4)}
   {:name "Drain"
    :cost 73
    :action #(-> % (damage :boss 2) (update-in [:me :hp] + 2))}
   {:name "Shield"
    :cost 113
    :action (fn [st]
              (update st
                      :effects
                      conj
                      {:name "Shield"
                       :turns 6
                       :action (fn [st] ; This is messy because it needs to read the state
                                 (let [active? (->> st :effects (find-key :name "Shield"))]
                                   (assoc-in st
                                             [:me :arm]
                                             (if active?
                                               7
                                               0))))}))}
   {:name "Poison"
    :cost 173
    :action (fn [st]
              (update st
                      :effects
                      conj
                      {:name "Poison"
                       :turns 6
                       :action #(update-in % [:boss :hp] - 3)}))}
   {:name "Recharge"
    :cost 229
    :action (fn [st]
              (update st
                      :effects
                      conj
                      {:name "Recharge"
                       :turns 5
                       :action #(update-in % [:me :mana] + 101)}))}])

;; Also removes effects when they reach 0 turns
(defn dec-effect-turns [st]
  (update st
          :effects
          (fn [es]
            (->> es
                 (map #(update % :turns dec))
                 (remove #(zero? (:turns %)))))))

(defn apply-effects [st]
  (let [f (->> st
               :effects
               (map :action)
               (apply comp))]
    ((comp f dec-effect-turns) st))) ; Decrement effect timers first

(defn swap-turn [st] (update st :turn {:me :boss :boss :me}))

;; TODO: Use Dijkstra's. Nodes are state, edges are mana cost.
(defn solve [st total-cost]
  (let [st (apply-effects st)]
    (cond
      (<= (get-in st [:boss :hp] st) 0) total-cost
      (<= (get-in st [:me :hp] st) 0) ##Inf
      :else (case (:turn st)
              :me (let [valid-spells (->> spells
                                          (filter #(and (<= (:cost %) (get-in st [:me :mana]))
                                                        (not (find-key :name
                                                                       (:name %)
                                                                       (:effects st))))))]
                    (if (empty? valid-spells)
                      ##Inf
                      (->> valid-spells
                           (map (fn [{:keys [action cost]}]
                                  (solve (-> st (update-in [:me :mana] - cost) action swap-turn)
                                         (+ total-cost cost))))
                           (apply min))))
              :boss (recur (-> st (damage :me (get-in st [:boss :dmg])) swap-turn)
                           total-cost)))))

(defn p1 [input]
  (let [init {:me {:hp 50 :arm 0 :mana 500}
              :boss (parse-input input)
              :effects []
              :turn :me}]
    (solve init 0)))

(defn p2 [input]
  (let [init {:me {:hp 50 :arm 0 :mana 500}
              :boss (parse-input input)
              :effects [{:name "Drip"
                         :turns ##Inf
                         :action #(case (:turn %)
                                    :me (update-in % [:me :hp] dec)
                                    :boss %)}]
              :turn :me}]
    (solve init 0)))