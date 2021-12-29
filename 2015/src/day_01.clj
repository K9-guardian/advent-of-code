(ns day-01)

(def input (slurp "input/d1.txt"))

(defn p1 [input]
  (->> input
       (map {\( 1 \) -1})
       (apply +)))

(defn p2 [input]
  (let [s (->> input
               (map {\( 1 \) -1})
               (reductions +))]
    (inc (.indexOf s -1))))