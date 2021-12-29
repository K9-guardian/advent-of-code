(ns day-25)

(def input (slurp "input/d25.txt"))

(defn parse-input [input]
  (let [nums (re-seq #"\d+" input)]
    (map #(Integer/parseInt %) nums)))

(defn coord->diagonal [[row col]]
  (let [x (/ (* col (inc col)) 2)] ; Triangle number
    (nth (reductions + x (iterate inc col)) (dec row))))

(defn next-code [code]
  (rem (* 252533 code)
       33554393))

(defn p1 [input]
  (let [init 20151125
        n (->> input
               parse-input
               coord->diagonal)]
    (nth (iterate next-code init) (dec n))))