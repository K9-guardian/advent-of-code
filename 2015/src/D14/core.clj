(ns D14.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d14.txt"))

(defn map-kv [f m] (reduce-kv (fn [m k v] (assoc m k (f v))) (empty m) m))

(defn parse-line [l]
  (->> l
       (re-find #"(.*) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")
       rest))

(defn parsed->map-entry [[raindeer & values]]
  (let [raindeer (keyword raindeer)
        values (map #(Integer/parseInt %) values)]
    [raindeer (zipmap [:speed :run-time :rest-time] values)]))

(defn stats->distance [time {:keys [speed run-time rest-time]}]
  (let [full-time (+ run-time rest-time)
        full-bursts (quot time full-time)
        part-burst (min run-time (rem time full-time))]
    (+ (* full-bursts speed run-time) (* part-burst speed))))

(defn next-second [m time]
  (let [deer-distances (->> m
                            (map-kv (fn [{:keys [stats] :as all}]
                                      (assoc all :distance (stats->distance time stats))))
                            (sort-by (comp :distance val) >)
                            (partition-by (comp :distance val)))
        leaders (map #(vector (key %) (update (val %) :points inc))
                     (first deer-distances))]
    (->> (rest deer-distances)
         (cons leaders)
         (apply concat)
         (into {}))))

(defn p1 [input]
  (->> input
       str/split-lines
       (map (comp parsed->map-entry parse-line))
       (into {})
       (map-kv (partial stats->distance 2503))
       (apply max-key val)))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map (comp (fn [[k v]] [k {:stats v :distance 0 :points 0}])
                          parsed->map-entry
                          parse-line))
               (into {}))
        m (reduce next-second m (range 1 2504))]
    (->> m
         (map-kv :points)
         (apply max-key val))))