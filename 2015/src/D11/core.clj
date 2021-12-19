(ns D11.core)

(def input (slurp "input/d11.txt"))

(defn increasing-3-straight? [s]
  (->>
   s
   (partition 3 1)
   (some (fn [[x y z]]
           (= (+ 2 (int x))
              (+ 1 (int y))
              (int z))))))

(defn contains-banned-letters? [s]
  (some #{\i \o \l} s))

(defn contains-2-different-non-overlap-pairs? [s]
  (->>
    s
    (partition-by identity)
    (filter #(= 2 (count %)))
    set
    count
    (= 2)))

(def valid?
  (every-pred
    increasing-3-straight?
    (complement contains-banned-letters?)
    contains-2-different-non-overlap-pairs?))

(defn inc-string [s]
  (letfn [(inc-char [c] (-> c int inc char))]
    (loop [[c & more :as cs] (reverse s)
           carry? true
           acc ()]
      (cond
        (and carry? (empty? cs)) (cons \a acc)
        (empty? cs) acc
        (and carry? (= \z c)) (recur more true (cons \a acc))
        carry? (recur more false (cons (inc-char c) acc))
        :else (recur more false (cons c acc))))))

(defn p1 [input]
  (->>
   input
   (iterate inc-string)
   (filter valid?)
   first
   (apply str)))

(defn p2 [input]
  (->>
   input
   (iterate inc-string)
   (filter valid?)
   second
   (apply str)))