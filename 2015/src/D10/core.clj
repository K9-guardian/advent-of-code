(ns D10.core)

(def input (slurp "input/d10.txt"))

(defn parse-input [s]
  (->>
   s
   (map #(Character/getNumericValue %))))

(defn next-turn [coll]
  (->>
   coll
   (partition-by identity)
   (mapcat #(list (count %) (first %)))))

(defn p1 [input]
  (as-> input $
    (parse-input $)
    (iterate next-turn $)
    (nth $ 40)
    (count $)))

(defn p2 [input]
  (as-> input $
    (parse-input $)
    (iterate next-turn $)
    (nth $ 50)
    (count $)))