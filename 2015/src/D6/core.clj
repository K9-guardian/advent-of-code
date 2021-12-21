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
    (let [parsed (->> s
                      (re-seq #"(turn on|turn off|toggle) (\d+,\d+) through (\d+,\d+)")
                      nfirst)
          [act c1 c2] parsed]
      [(m act)
       (mapv #(Integer/parseInt %) (str/split c1 #","))
       (mapv #(Integer/parseInt %) (str/split c2 #","))])))

(defn update!-inclusive-range-longs [^longs arr idx1 idx2 f]
  (doseq [i (range idx1 (inc idx2))]
    (aset arr i ^long (f (aget arr i)))))

(defn update!-grid [^longs arr [x1 x2] [y1 y2] f]
  (doseq [i (range y1 (inc y2))]
    (update!-inclusive-range-longs
     arr
     (+ (* 1000 i) x1)
     (+ (* 1000 i) x2)
     f)))

(defn p1 [input]
  (let [init (long-array 1000000 0)
        parsed (->> input
                    str/split-lines
                    (map (parse-instruction action->function-p1)))]
    (doseq [[act [x1 y1] [x2 y2]] parsed]
      (update!-grid init [x1 x2] [y1 y2] act))
    (areduce init idx ret 0 (+ ret (aget init idx)))))

(defn p2 [input]
  (let [init (long-array 1000000 0)
        parsed (->> input
                    str/split-lines
                    (map (parse-instruction action->function-p2)))]
    (doseq [[act [x1 y1] [x2 y2]] parsed]
      (update!-grid init [x1 x2] [y1 y2] act))
    (areduce init idx ret 0 (+ ret (aget init idx)))))