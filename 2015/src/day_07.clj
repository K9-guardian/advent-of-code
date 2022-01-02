(ns day-07
  (:require [clojure.string :as str]))

(def input (slurp "input/d7.txt"))

(defn parse-atom [a]
  (try
    (Integer/parseInt a)
    (catch NumberFormatException e (keyword a))))

;; instruction ::= lhs "->" rhs
;; rhs ::= symb
;; lhs ::= atom | "NOT" atom | atom ("AND" | "OR" | "LSHIFT" | "RSHIFT") atom

;; atom ::= symb | num
;; symb ::= [a-z]+
;; num ::= \d+

(defn parse-line [l]
  (let [[lhs rhs] (str/split l #" -> ")
        rhs (parse-atom rhs)
        lhs (map parse-atom (str/split lhs #" "))]
    [rhs
     (cond
       (= :NOT (first lhs)) (as-> lhs [f x] {:f f :x x})
       (#{:AND :OR :LSHIFT :RSHIFT} (second lhs)) (as-> lhs [x f y] {:f f :x x :y y})
       :else (as-> lhs [x] {:x x}))]))

(defn clamp [f] (comp (partial bit-and 0xFFFF) f))

(defn evaluate [k m]
  ;; I swear to learn fixed point memoization *eventually*
  (def eval-rec
    (memoize
     (fn [k]
       (if (number? k)
         k
         (let [{:keys [f x y]} (m k)]
           (case f
             :NOT ((clamp bit-not) (eval-rec x))
             :AND ((clamp bit-and) (eval-rec x) (eval-rec y))
             :OR ((clamp bit-or) (eval-rec x) (eval-rec y))
             :LSHIFT ((clamp bit-shift-left) (eval-rec x) (eval-rec y))
             :RSHIFT ((clamp bit-shift-right) (eval-rec x) (eval-rec y))
             (eval-rec x)))))))
  (eval-rec k))

(defn p1 [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into {})
       (evaluate :a)))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (into {}))
        m (assoc m :b {:x (evaluate :a m)})]
    (evaluate :a m)))