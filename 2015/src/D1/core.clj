(ns D1.core)

(def input (slurp "input/d1.txt"))

(defn p1 [input]
  (->> input
       (map #(case %
               \( 1
               \) -1))
       (apply +)))

(defn p2 [input]
  (let [seqq (->> input
               (map #(case %
                       \( 1
                       \) -1))
               (reductions +))]
    (-> seqq (.indexOf -1) inc)))