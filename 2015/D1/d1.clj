(def input (slurp "d1.txt"))

(defn p1 [input]
  (transduce
   (map #(case %
           \( 1
           \) -1))
   +
   input))

(defn p2 [input]
  (reduce
   (fn [[f x] v]
     (if (= -1 x)
       (reduced f)
       (vector
        (inc f)
        (+
         x
         (case v
           \( 1
           \) -1)))))
   [0 0]
   input)
  #_(as-> input $
      (map #(case %
              \( 1
              \) -1)
           $)
      (reductions + $)
      (.indexOf $ -1)
      (inc $)))