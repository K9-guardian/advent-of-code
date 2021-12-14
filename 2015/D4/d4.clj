(import java.security.MessageDigest)
(require '[clojure.core.reducers :as r])

(def input (slurp "d4.txt"))

;; Good general purpose algorithm, but slow for day 2
(def algo (MessageDigest/getInstance "MD5"))
(defn md5 [^String input]
  (->>
   (.getBytes input "UTF-8")
   (.digest algo)
   (BigInteger. 1)
   (format "%032x")))

(defn budget-md5-6-zeros-prefix? [^String input]
  (->>
    (.getBytes input "UTF-8")
    (.digest algo)
    #_(.digest (MessageDigest/getInstance "MD5")) ; Use this for parallel solve
    (take 3)
    (every? zero?)))

(defn d1 [input]
  (loop [i 1]
    (if (every? #{\0} (take 5 (md5 (str input i))))
      i
      (recur (inc i)))))

(defn d2 [input]
  (loop [i 1]
    (if (budget-md5-6-zeros-prefix? (str input i))
      i
      (recur (inc i)))))

;; Attempt to parallelize
#_(defn d2 [input]
    (as-> (iterate inc 1) $
      (pmap
       (comp
        budget-md5-6-zeros-prefix?
        (partial str input))
       $)
      (.indexOf $ true)
      (inc $)))