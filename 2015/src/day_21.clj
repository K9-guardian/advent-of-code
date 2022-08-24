(ns day-21
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(def input (slurp "input/d21.txt"))

(def weapons
  [{:type "Dagger"     :cost  8 :dmg 4 :arm 0}
   {:type "Shortsword" :cost 10 :dmg 5 :arm 0}
   {:type "Warhammer"  :cost 25 :dmg 6 :arm 0}
   {:type "Longsword"  :cost 40 :dmg 7 :arm 0}
   {:type "Greataxe"   :cost 74 :dmg 8 :arm 0}])

(def armor
  [{:type "Leather"    :cost  13 :dmg 0 :arm 1}
   {:type "Chainmail"  :cost  31 :dmg 0 :arm 2}
   {:type "Splintmail" :cost  53 :dmg 0 :arm 3}
   {:type "Bandedmail" :cost  75 :dmg 0 :arm 4}
   {:type "Platemail"  :cost 102 :dmg 0 :arm 5}])

(def rings
  [{:type "Damage +1"  :cost  25 :dmg 1 :arm 0}
   {:type "Damage +2"  :cost  50 :dmg 2 :arm 0}
   {:type "Damage +3"  :cost 100 :dmg 3 :arm 0}
   {:type "Defense +1" :cost  20 :dmg 0 :arm 1}
   {:type "Defense +2" :cost  40 :dmg 0 :arm 2}
   {:type "Defense +3" :cost  80 :dmg 0 :arm 3}])

(def parse-key
  {"Hit Points" :hp
   "Damage" :dmg
   "Armor" :arm})

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(str/split % #": "))
       (map (fn [[k v]] [(parse-key k) (Integer/parseInt v)]))
       (into {})))

(defn win? [boss [weapon armor rings]]
  (let [dmg (-> (+ (:dmg weapon) (apply + (map :dmg rings)))
                (- (:arm boss))
                (max 1))
        boss-dmg (-> (:dmg boss)
                     (- (+ (:arm armor) (apply + (map :arm rings))))
                     (max 1))
        turns-to-win (Math/ceil (/ (:hp boss) dmg))
        turns-to-lose (Math/ceil (/ 100 boss-dmg))]
    (<= turns-to-win turns-to-lose)))

(defn cost [[weapon armor rings]]
  (+ (:cost weapon)
     (:cost armor)
     (apply + (map :cost rings))))

(defn p1 [input]
  (let [boss (parse-input input)
        loadouts (comb/cartesian-product weapons
                                         (conj armor {:type :none :cost 0 :dmg 0 :arm 0})
                                         (->> rings comb/subsets (take-while #(<= (count %) 2))))]
    (->> loadouts
         (filter (partial win? boss))
         (map (juxt identity cost))
         (apply min-key second))))

(defn p2 [input]
  (let [boss (parse-input input)
        loadouts (comb/cartesian-product weapons
                                         (conj armor {:type :none :cost 0 :dmg 0 :arm 0})
                                         (->> rings comb/subsets (take-while #(<= (count %) 2))))]
    (->> loadouts
         (filter (partial (complement win?) boss))
         (map (juxt identity cost))
         (apply max-key second))))
