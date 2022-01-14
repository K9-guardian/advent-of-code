(ns day-10)

(def input (slurp "input/d10.txt"))

(defn parse-input [s]
  (map #(Character/getNumericValue %) s))

(defn next-turn [coll]
  (->> coll
       (partition-by identity)
       (mapcat #(list (count %) (first %)))))

(defn p1 [input]
  (let [s (->> input parse-input (iterate next-turn))]
    (count (nth s 40))))

(defn p2 [input]
  (let [s (->> input parse-input (iterate next-turn))]
    (count (nth s 50))))