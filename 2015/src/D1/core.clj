(ns D1.core)

(def input (slurp "d1.txt"))

(defn p1 [input]
  (transduce
   (map #(case %
           \( 1
           \) -1))
   +
   input))

(defn p2 [input]
  (as-> input $
    (map #(case %
            \( 1
            \) -1)
         $)
    (reductions + $)
    (.indexOf $ -1)
    (inc $)))