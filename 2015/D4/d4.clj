(import java.security.MessageDigest)
(require '[clojure.core.reducers :as r])

(def input (slurp "d4.txt"))

;; Good general purpose algorithm, but slow for day 2
(def algo (MessageDigest/getInstance "MD5"))
(defn md5 ^String [^String input]
  (->>
   (.getBytes input "UTF-8")
   (.digest algo)
   (BigInteger. 1)
   (format "%032x")))

;; You could further optimize this by reusing buffers, but I'm lazy
(defn budget-md5-6-zeros-prefix? [^String input]
    (as-> input $
      (.getBytes $ "UTF-8")
      (.digest ^MessageDigest algo $)
      (and (zero? (aget $ 0))
           (zero? (aget $ 1))
           (zero? (aget $ 2)))))

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