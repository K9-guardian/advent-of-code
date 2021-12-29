(ns day-14
  (:require [clojure.string :as str]))

(def input (slurp "input/d14.txt"))

(defn parse-line [l]
  (let [[name & values] (rest
                         (re-find
                          #"(.*) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."
                          l))
        values (->> values
                    (map #(Integer/parseInt %))
                    (zipmap [:speed :run-time :rest-time]))]
    (assoc values :name name)))

(defn stats->distance [time {:keys [speed run-time rest-time]}]
  (let [full-time (+ run-time rest-time)
        full-bursts (quot time full-time)
        part-burst (min run-time (rem time full-time))]
    (+ (* full-bursts speed run-time) (* part-burst speed))))

(defn next-second [deer time]
  (let [deer-buckets (->> deer
                          (map #(assoc % :distance (stats->distance time (:stats %))))
                          (sort-by :distance >)
                          (partition-by :distance))]
    (->> (first deer-buckets)
         (map #(update % :points inc))
         (conj (rest deer-buckets))
         (apply concat))))

(defn p1 [input]
  (->> input
       str/split-lines
       (map parse-line)
       (map (juxt :name (partial stats->distance 2503)))
       (apply max-key second)))

(defn p2 [input]
  (let [deer (->> input
                  str/split-lines
                  (map (comp (fn [{name :name :as all}]
                               {:name name
                                :stats (dissoc all :name)
                                :distance 0
                                :points 0})
                             parse-line)))
        deer (reduce next-second deer (range 1 2504))]
    (->> deer
         (map (juxt :name :points))
         (apply max-key second))))