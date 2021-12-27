(ns D10.core)

(def input (slurp "input/d10.txt"))

(defn parse-input [s]
  (map #(Character/getNumericValue %) s))

(defn next-turn [coll]
  (->> coll
       (partition-by identity)
       (mapcat #(list (count %) (first %)))))

(defn p1 [input]
  (let [seqq (->> input
                  parse-input
                  (iterate next-turn))]
    (count (nth seqq 40))))

(defn p2 [input]
  (let [seqq (->> input
                  parse-input
                  (iterate next-turn))]
    (count (nth seqq 50))))