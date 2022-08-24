(ns day-22
  (:require [clojure.string :as str])
  (:use priority-queue))

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

(def effects
  {"Shield" #(assoc-in % [:me :arm] (if (-> % :effects (get "Shield")) 7 0))
   "Poison" #(update-in % [:boss :hp] - 3)
   "Recharge" #(update-in % [:me :mana] + 101)
   "Drip" #(if (= :my-start (:turn %)) (update-in % [:me :hp] dec) %)})

(def spells
  [{:name "Magic Missile" :cost 53 :action #(update-in % [:boss :hp] - 4)}
   {:name "Drain" :cost 73 :action #(-> % (update-in [:boss :hp] - 2) (update-in [:me :hp] + 2))}
   {:name "Shield" :cost 113 :action #(update % :effects assoc "Shield" 6)}
   {:name "Poison" :cost 173 :action #(update % :effects assoc "Poison" 6)}
   {:name "Recharge" :cost 229 :action #(update % :effects assoc "Recharge" 5)}])

;; Also removes effects when they reach 0 turns
(defn dec-effect-turns [st]
  (update st
          :effects
          (partial into
                   {}
                   (comp (map #(update % 1 dec))
                         (remove #(-> % second zero?))))))

(defn swap-turn [st]
  (update st
          :turn
          {:me :boss-start
           :boss-start :boss
           :boss :my-start
           :my-start :me}))

(defn dijkstra [pq dist]
  (if (empty? pq)
    dist
    (let [[[st p] pq] [(peek pq) (pop pq)]]
      (if (or (not= p (dist st))
              (<= (-> st :boss :hp) 0)
              (<= (-> st :me :hp) 0))
        (recur pq dist)
        (let [edges (case (:turn st) ; [[cost function] ...]
                      (:my-start :boss-start) [[0 (->> dec-effect-turns
                                                       (comp (->> st
                                                                  :effects
                                                                  keys
                                                                  (map effects)
                                                                  (apply comp))))]]
                      :boss [[0 #(let [amt (max 1 (- (-> % :boss :dmg) (-> % :me :arm)))]
                                   (update-in % [:me :hp] - amt))]]
                      :me (->> spells
                               (filter #(and (<= (:cost %) (-> st :me :mana))
                                             (not (-> st :effects (get (:name %))))))
                               (map (fn [{:keys [cost action]}]
                                      [cost (comp action #(update-in % [:me :mana] - cost))]))))]
          (-> edges
              (->> (reduce
                    (fn [[pq dist] [cost f]]
                      (let [st (-> st f swap-turn)
                            alt (+ cost p)]
                        (if (< alt (dist st ##Inf))
                          [(conj pq [st alt]) (conj dist [st alt])]
                          [pq dist])))
                    [pq dist]))
              (as-> [pq dist] (recur pq dist))))))))

(defn p1 [input]
  (let [init {:me {:hp 50 :arm 0 :mana 500}
              :boss (parse-input input)
              :effects {}
              :turn :me}]
    (->> (dijkstra (priority-queue init 0) {init 0})
         (filter (comp #(<= (-> % :boss :hp) 0) key))
         (apply min-key val))))

(defn p2 [input]
  (let [init {:me {:hp 50 :arm 0 :mana 500}
              :boss (parse-input input)
              :effects {"Drip" ##Inf}
              :turn :me}]
    (->> (dijkstra (priority-queue init 0) {init 0})
         (filter (comp #(<= (-> % :boss :hp) 0) key))
         (apply min-key val))))
