(ns day-20)

(def input (Integer/parseInt (slurp "input/d20.txt")))

(defn p1 [input]
  (reduce (fn [m n]
            (if (<= input (* 10 (apply + (m n))))
              (reduced n)
              (reduce (fn [m d] (update m (+ n d) (fnil conj [(+ n d)]) d))
                      m
                      (m n))))
          {1 [1]}
          (iterate inc 1)))

(defn p2 [input]
  (reduce (fn [m n]
            (if (<= input (->> (m n)
                               (filter #(< n (* 50 %)))
                               (apply +)
                               (* 11)))
              (reduced n)
              (reduce (fn [m d] (update m (+ n d) (fnil conj [(+ n d)]) d))
                      m
                      (m n))))
          {1 [1]}
          (iterate inc 1)))