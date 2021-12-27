(ns D15.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d15.txt"))

(defn parse-line [l]
  (let [[name & stats] (str/split l #"[:,] ")
        properties (->> stats
                        (map (comp
                              (fn [[k v]] [(keyword k) (Integer/parseInt v)])
                              #(str/split % #" ")))
                        (into {}))]
    (assoc properties :name name)))

(defn ratio->score [ingredients ratio]
  (->> ingredients
       (map (juxt :capacity :durability :flavor :texture))
       (apply map list)
       (map #(->> %
                  (map * ratio)
                  (apply +)
                  (max 0)))
       (apply *)))

(defn ratio->calories [ingredients ratio]
  (->> ingredients
       (map :calories)
       (map * ratio)
       (apply +)))

(def ratios
  (for [i (range 0 101) j (range 0 101) k (range 0 101) l (range 0 101)
        :when (= 100 (+ i j k l))]
    [i j k l]))

(defn p1 [input]
  (let [ingredients (->> input
                         str/split-lines
                         (map parse-line))]
    (->> ratios
         (map (juxt (partial zipmap (map :name ingredients))
                    (partial ratio->score ingredients)))
         (apply max-key second))))

(defn p2 [input]
  (let [ingredients (->> input
                         str/split-lines
                         (map parse-line))]
    (->> ratios
         (filter #(= 500 (ratio->calories ingredients %)))
         (map (juxt (partial zipmap (map :name ingredients))
                    (partial ratio->score ingredients)))
         (apply max-key second))))