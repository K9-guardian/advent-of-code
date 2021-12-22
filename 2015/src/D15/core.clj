(ns D15.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d15.txt"))

(defn parse-line [l]
  (let [[ingredient & more] (str/split l #"[:,] ")
        properties (->> more
                        (map (comp
                              (fn [[k v]] [(keyword k) (Integer/parseInt v)])
                              #(str/split % #" ")))
                        (into {}))]
    [(keyword ingredient) properties]))

(defn ratio->score [xs properties-per-teaspoon]
  (->> properties-per-teaspoon
       (apply map (fn [p1 p2 p3 p4]
                    (max 0 (->> (map * [p1 p2 p3 p4] xs)
                                (apply +)))))
       (apply *)))

(defn ratio->calories [xs cals]
  (->> (map * cals xs)
       (apply +)))

(def nums
  (for [i (range 0 101) j (range 0 101)
        k (range 0 101) l (range 0 101)
        :when (= 100 (+ i j k l))
        ]
    [i j k l]))

(defn p1 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (into {}))
        number-of-ingredients (count m)]
    (->
     (for [xs nums]
       [xs
        (ratio->score xs
                      (->> m vals
                           (map (fn [{:keys [capacity durability flavor texture]}]
                                  [capacity durability flavor texture]))))])
     (->> (apply max-key second))
     (as-> [xs score] [(zipmap (keys m) xs) score]))))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (into {}))
        number-of-ingredients (count m)]
    (->
     (for [xs nums
           :when (= 500 (ratio->calories xs (map :calories (vals m))))]
       [xs
        (ratio->score xs
                      (->> m vals
                           (map (fn [{:keys [capacity durability flavor texture]}]
                                  [capacity durability flavor texture]))))])
     (->> (apply max-key second))
     (as-> [xs score] [(zipmap (keys m) xs) score]))))