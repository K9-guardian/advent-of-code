(ns D7.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d7.txt"))

(defn parse-atom [a]
  (try
    (Integer/parseInt a)
    (catch NumberFormatException e (keyword a))))

;; instruction ::= lhs "->" rhs
;; rhs ::= symb
;; lhs ::= fn atom | atom fn atom

;; fn ::= "NOT" | "AND" | "OR" | "LSHIFT" | "RSHIFT"
;; atom ::= symb | num
;; symb ::= [a-z]+
;; num ::= \d+

(defn parse-instruction [s]
  (let [[lhs rhs] (str/split s #" -> ")
        rhs (parse-atom rhs)
        lhs (mapv parse-atom (str/split lhs #" "))]
    [rhs lhs]))

(defn clamp [f] (comp (partial bit-and 0xFFFF) f))

(def act->fn
  {:NOT (clamp bit-not)
   :AND (clamp bit-and)
   :OR (clamp bit-or)
   :LSHIFT (clamp bit-shift-left)
   :RSHIFT (clamp bit-shift-right)})

(defn evaluate [k m]
  ;; I swear to learn fixed point memoization *eventually*
  (def eval-rec
    (memoize
     (fn [k]
       (if (number? k)
         k
         (let [v (m k)]
           (case (count v)
             1 (as-> v [x] (eval-rec x))
             2 (as-> v [f x] ((act->fn f) (eval-rec x)))
             3 (as-> v [x f y] ((act->fn f) (eval-rec x) (eval-rec y)))))))))
  (eval-rec k))

(defn p1 [input]
  (->> input
       str/split-lines
       (map parse-instruction)
       (into {})
       (evaluate :a)))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map parse-instruction)
               (into {}))
        m (assoc m :b [(evaluate :a m)])]
    (evaluate :a m)))