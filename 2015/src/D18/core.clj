(ns D18.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d18.txt"))

(def board-size 100)

(def ranges
  (for [i (range -1 2)
        j (range -1 2)
        :when (not= 0 i j)]
    [i j]))

(defn in-bounds? [pair]
  (let [i (first pair)
        j (second pair)]
    (and (< -1 i board-size)
         (< -1 j board-size))))

(defn next-frame-p1 [lights]
  (->> (for [i (range board-size) j (range board-size)
             :let [l (get-in lights [i j])
                   n (transduce (comp (map (partial map + [i j]))
                                      (filter in-bounds?)
                                      (map (partial get-in lights)))
                                +
                                ranges)]]
         (cond
           (and (= 1 l) (<= 2 n 3)) 1
           (and (= 0 l) (= 3 n)) 1
           :else 0))
       (partition board-size)
       (mapv vec)))

(defn corner? [pair]
  (let [i (first pair)
        j (second pair)]
    (and (or (= 0 i)
             (= (dec board-size) i))
         (or (= 0 j)
             (= (dec board-size) j)))))

(defn next-frame-p2 [lights]
  (->> (for [i (range board-size) j (range board-size)
             :let [l (get-in lights [i j])
                   n (transduce (comp (map (partial map + [i j]))
                                      (filter in-bounds?)
                                      (map (partial get-in lights)))
                                +
                                ranges)]]
         (cond
           (corner? [i j]) 1
           (and (= 1 l) (<= 2 n 3)) 1
           (and (= 0 l) (= 3 n)) 1
           :else 0))
       (partition board-size)
       (mapv vec)))

(defn p1 [input]
  (let [lights (->> input
                    str/split-lines
                    (mapv (partial mapv #(case % \. 0 \# 1))))
        frame (nth (iterate next-frame-p1 lights) 100)]
    (->> frame
         flatten
         (filter (partial = 1))
         count)))

(defn p2 [input]
  (let [lights (->> input
                    str/split-lines
                    (mapv (partial mapv #(case % \. 0 \# 1))))
        lights (-> lights
                   (assoc-in [0 0] 1)
                   (assoc-in [0 (dec board-size)] 1)
                   (assoc-in [(dec board-size) 0] 1)
                   (assoc-in [(dec board-size) (dec board-size)] 1))
        frame (nth (iterate next-frame-p2 lights) 100)]
    (->> frame
         flatten
         (filter (partial = 1))
         count)))