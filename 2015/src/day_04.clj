(ns day-04
  (:import java.security.MessageDigest))

(def input (slurp "input/d4.txt"))

;; Good general purpose algorithm, but slow for day 2
(defn md5 [input]
  (->> (.getBytes input "UTF-8")
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

;; You could further optimize this by reusing buffers, but type hints are good enough
(defn budget-md5-6-zeros-prefix? [^String input]
  (let [bs (->> (.getBytes input "UTF-8")
                (.digest ^MessageDigest (MessageDigest/getInstance "MD5")))]
    (and (zero? (aget bs 0))
         (zero? (aget bs 1))
         (zero? (aget bs 2)))))

(defn p1 [input]
  (loop [i 1]
    (if (every? #{\0} (take 5 (md5 (str input i))))
      i
      (recur (inc i)))))

(defn p2 [input]
  (let [l (->> (iterate inc 1)
               (pmap
                (comp budget-md5-6-zeros-prefix?
                      (partial str input))))]
    (inc (.indexOf l true))))