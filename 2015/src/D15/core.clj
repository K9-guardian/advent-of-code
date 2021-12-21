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

;; dp[][][][]

(defn sum-to-n [n x]
  (case x
    1 [[n]]
    (partition x
               (flatten
                (for [i (range (inc n))
                      :let [nums (sum-to-n (- n i) (dec x))]]
                  (map (partial cons i) nums))))))

(defn ratio->score [xs properties-per-teaspoon]
  (->> properties-per-teaspoon
       (apply map (fn [p1 p2 p3 p4]
                    (max 0 (->> (map * [p1 p2 p3 p4] xs)
                                (apply +)))))
       (apply *)))

(defn ratio->calories [xs cals]
  (->> (map * cals xs)
       (apply +)))

(defn p1 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (into {}))
        number-of-ingredients (count m)]
    (->
     (for [xs (sum-to-n 100 number-of-ingredients)]
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
     (for [xs (sum-to-n 100 number-of-ingredients)
           :when (= 500 (ratio->calories xs (map :calories (vals m))))]
       [xs
        (ratio->score xs
                      (->> m vals
                           (map (fn [{:keys [capacity durability flavor texture]}]
                                  [capacity durability flavor texture]))))])
     (->> (apply max-key second))
     (as-> [xs score] [(zipmap (keys m) xs) score]))))