(ns day-02
  (:require [clojure.string :as str]))

(def input (slurp "input/d2.txt"))

(defn box->area [l w h]
  (+ (* 2 l w)
     (* 2 w h)
     (* 2 h l)
     (min (* l w)
          (* w h)
          (* h l))))

(defn box->ribbon [l w h]
  (apply +
         (* l w h)
         (take 2 (sort < [(* 2 l) (* 2 w) (* 2 h)]))))

(defn p1 [input]
  (->> input
       str/split-lines
       (map (comp (partial apply box->area)
                  (partial map #(Integer/parseInt %))
                  #(str/split % #"x")))
       (apply +)))

(defn p2 [input]
  (->> input
       str/split-lines
       (map (comp (partial apply box->ribbon)
                  (partial map #(Integer/parseInt %))
                  #(str/split % #"x")))
       (apply +)))
