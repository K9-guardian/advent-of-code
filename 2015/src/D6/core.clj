(ns D6.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d6.txt"))

(def action->function-p1
  {"turn on" (partial bit-or 1),
   "turn off" (partial bit-and 0),
   "toggle" #(bit-flip % 0)})

(def action->function-p2
  {"turn on" inc,
   "turn off" #(max 0 (dec %)),
   "toggle" (partial + 2)})

(defn parse-instruction [m]
  (fn [s]
    (as-> s $
      (re-seq #"(turn on|turn off|toggle) (\d+,\d+) through (\d+,\d+)" $)
      (nfirst $)
      (as-> $ [act c1 c2]
        [(m act)
         (mapv #(Integer/parseInt %) (str/split c1 #","))
         (mapv #(Integer/parseInt %) (str/split c2 #","))]))))

(defn update-inclusive-range [v start end f]
  (loop [i start v v]
    (if (> i end)
      v
      (recur (inc i) (update v i f)))))

(defn next-state [state [act [x1 y1] [x2 y2]]]
  (update-inclusive-range
   state
   y1
   y2
   #(update-inclusive-range % x1 x2 act)))

(def init
  (vec
   (repeat
    1000
    (vec (repeat 1000 0)))))

(defn p1 [input]
  (->>
   input
   str/split-lines
   (map (parse-instruction action->function-p1))
   (reduce next-state init)
   flatten
   (filter (complement zero?))
   count))

(defn p2 [input]
  (->>
   input
   str/split-lines
   (map (parse-instruction action->function-p2))
   (reduce next-state init)
   flatten
   (reduce +)))