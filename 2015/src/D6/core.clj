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

;; instruction ::= action range "through" range
;; action ::= "turn on" | "turn off" | "toggle"
;; range ::= \d+,\d+

(defn parse-instruction [m]
  (fn [s]
    (let [[act c1 c2] (->> s
                           (re-find #"(turn on|turn off|toggle) (\d+,\d+) through (\d+,\d+)")
                           rest)]
      {:action (m act)
       :from-range (mapv #(Integer/parseInt %) (str/split c1 #","))
       :to-range (mapv #(Integer/parseInt %) (str/split c2 #","))})))

(defn update!-inclusive-range [^longs arr i1 i2 f]
  (doseq [i (range i1 (inc i2))]
    (aset arr i ^long (f (aget arr i)))))

(defn update!-grid [^longs arr x1 x2 y1 y2 f]
  (doseq [i (range y1 (inc y2))]
    (update!-inclusive-range arr
                             (+ (* 1000 i) x1)
                             (+ (* 1000 i) x2)
                             f)))

(defn p1 [input]
  (let [init (long-array 1000000 0)
        parsed (->> input
                    str/split-lines
                    (map (parse-instruction action->function-p1)))]
    (doseq [{act :action [x1 y1] :from-range [x2 y2] :to-range} parsed]
      (update!-grid init x1 x2 y1 y2 act))
    (areduce init i ret 0 (+ ret (aget init i)))))

(defn p2 [input]
  (let [init (long-array 1000000 0)
        parsed (->> input
                    str/split-lines
                    (map (parse-instruction action->function-p2)))]
    (doseq [{act :action [x1 y1] :from-range [x2 y2] :to-range} parsed]
      (update!-grid init x1 x2 y1 y2 act))
    (areduce init i ret 0 (+ ret (aget init i)))))