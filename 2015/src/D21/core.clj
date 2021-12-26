(ns D21.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(def input (slurp "input/d21.txt"))

(def weapons
  [{:type :dagger     :cost  8 :dmg 4 :arm 0}
   {:type :shortsword :cost 10 :dmg 5 :arm 0}
   {:type :warhammer  :cost 25 :dmg 6 :arm 0}
   {:type :longsword  :cost 40 :dmg 7 :arm 0}
   {:type :greataxe   :cost 74 :dmg 8 :arm 0}])

(def armor
  [{:type :leather    :cost  13 :dmg 0 :arm 1}
   {:type :chainmail  :cost  31 :dmg 0 :arm 2}
   {:type :splintmail :cost  53 :dmg 0 :arm 3}
   {:type :bandedmail :cost  75 :dmg 0 :arm 4}
   {:type :platemail  :cost 102 :dmg 0 :arm 5}])

(def rings
  [{:type :damage+1  :cost  25 :dmg 1 :arm 0}
   {:type :damage+2  :cost  50 :dmg 2 :arm 0}
   {:type :damage+3  :cost 100 :dmg 3 :arm 0}
   {:type :defense+1 :cost  20 :dmg 0 :arm 1}
   {:type :defense+2 :cost  40 :dmg 0 :arm 2}
   {:type :defense+3 :cost  80 :dmg 0 :arm 3}])

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
  (let [dmg (-> (+ (:dmg weapon)
                   (apply + (map :dmg rings)))
                (- (:arm boss))
                (max 1))
        boss-dmg (-> (:dmg boss)
                     (- (+ (:arm armor)
                           (apply + (map :arm rings))))
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
                                         (-> armor
                                             (conj {:type :none :cost 0 :dmg 0 :arm 0}))
                                         (->> rings
                                              comb/subsets
                                              (take-while #(<= (count %) 2))))]
    (->> loadouts
         (filter (partial win? boss))
         (map (fn [l] [l (cost l)]))
         (apply min-key second))))

(defn p2 [input]
  (let [boss (parse-input input)
        loadouts (comb/cartesian-product weapons
                                         (-> armor
                                             (conj {:type :none :cost 0 :dmg 0 :arm 0}))
                                         (->> rings
                                              comb/subsets
                                              (take-while #(<= (count %) 2))))]
    (->> loadouts
         (filter (partial (complement win?) boss))
         (map (fn [l] [l (cost l)]))
         (apply max-key second))))