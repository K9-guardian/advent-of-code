(ns day-11)

(def input (slurp "input/d11.txt"))

(defn increasing-3-straight? [s]
  (->> s
       (partition 3 1)
       (some #(->> %
                   (map int)
                   (partition 2 1)
                   (map (partial apply -))
                   (every? #{-1})))))

(defn contains-banned-letters? [s]
  (some #{\i \o \l} s))

(defn contains-2-different-non-overlap-pairs? [s]
  (->> s
       (partition-by identity)
       (filter #(= 2 (count %)))
       set
       count
       (= 2)))

(def valid?
  (every-pred increasing-3-straight?
              (complement contains-banned-letters?)
              contains-2-different-non-overlap-pairs?))

(defn inc-string [s]
  (letfn [(inc-char [c] (-> c int inc char))]
    (loop [[c & cs :as all] (reverse s)
           carry? true
           acc ()]
      (cond
        (and carry? (empty? all)) (cons \a acc)
        (empty? all) acc
        (and carry? (= \z c)) (recur cs true (cons \a acc))
        carry? (recur cs false (cons (inc-char c) acc))
        :else (recur cs false (cons c acc))))))

(defn p1 [input]
  (->> input
       (iterate inc-string)
       (filter valid?)
       first
       (apply str)))

(defn p2 [input]
  (->> input
       (iterate inc-string)
       (filter valid?)
       second
       (apply str)))