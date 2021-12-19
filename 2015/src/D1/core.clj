(ns D1.core)

(def input (slurp "input/d1.txt"))

(defn p1 [input]
  (->>
   input
   (map #(case %
           \( 1
           \) -1))
   (apply +)))

(defn p2 [input]
  (as-> input $
    (map #(case %
            \( 1
            \) -1)
         $)
    (reductions + $)
    (.indexOf $ -1)
    (inc $)))