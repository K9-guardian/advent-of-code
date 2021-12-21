(ns D7.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d7.txt"))

(defn parse-atom [a]
  (try
    (Integer/parseInt a)
    (catch NumberFormatException e (keyword a))))

(defn parse-instruction [s]
  (->> s
       (re-seq #"(?:([a-z\d]+)|([a-z\d]+)? ?([A-Z]+) ([a-z\d]+)) -> ([a-z]+)")
       nfirst
       (remove nil?)
       (map parse-atom)))

(defn instruction->map-entry
  ([x y] [y [x]])
  ([f x y] [y [f x]])
  ([x f y z] [z [f x y]]))

(defn clamp [f] (comp (partial bit-and 0xFFFF) f))

(defn evaluate [m k]
  ;; I swear to learn combinators *eventually*
  (def eval-rec
    (memoize
     (fn [k]
       (if (number? k)
         k
         (let [[g x y] (m k)]
           (case g
             :NOT ((clamp bit-not) (eval-rec x))
             :AND ((clamp bit-and) (eval-rec x) (eval-rec y))
             :OR ((clamp bit-or) (eval-rec x) (eval-rec y))
             :LSHIFT ((clamp bit-shift-left) (eval-rec x) (eval-rec y))
             :RSHIFT ((clamp bit-shift-right) (eval-rec x) (eval-rec y))
             (eval-rec g)))))))
  (eval-rec k))

(defn p1 [input]
  (let [m (->> input
               str/split-lines
               (map (comp (partial apply instruction->map-entry)
                          parse-instruction))
               (into {}))]
    (evaluate m :a)))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map (comp (partial apply instruction->map-entry)
                          parse-instruction))
               (into {}))]
    (-> m
        (assoc :b [(evaluate m :a)])
        (evaluate :a))))