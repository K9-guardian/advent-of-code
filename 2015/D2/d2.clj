(require '[clojure.string :as str])
(def input (slurp "d2.txt"))

(defn box->area [l w h]
  (+
   (* 2 l w)
   (* 2 w h)
   (* 2 h l)
   (min
    (* l w)
    (* w h)
    (* h l))))

(defn box->ribbon [l w h]
  (apply
   +
   (* l w h)
   (take 2 (sort < [(* 2 l) (* 2 w) (* 2 h)]))))

(defn d1 [input]
  (->>
   input
   str/split-lines
   (transduce
    (map
     (comp
      (partial apply box->area)
      (partial map #(Integer/parseInt %))
      nfirst
      (partial re-seq #"(\d+)x(\d+)x(\d+)")))
    +)))

(defn d2 [input]
  (->>
   input
   str/split-lines
   (transduce
    (map
     (comp
      (partial apply box->ribbon)
      (partial map #(Integer/parseInt %))
      nfirst
      (partial re-seq #"(\d+)x(\d+)x(\d+)")))
    +)))