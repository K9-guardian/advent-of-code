(ns day-07
  (:require [clojure.string :as str]))

(def input (slurp "input/d7.txt"))

(defn parse-atom [a]
  (try
    (Integer/parseInt a)
    (catch NumberFormatException e a)))

;; instruction ::= lhs "->" rhs
;; rhs ::= symb
;; lhs ::= atom | "NOT" atom | atom ("AND" | "OR" | "LSHIFT" | "RSHIFT") atom

;; atom ::= symb | num
;; symb ::= [a-z]+
;; num ::= \d+

(defn parse-line [l]
  (let [[lhs rhs] (str/split l #" -> ")
        lhs (str/split lhs #" ")]
    [rhs
     (condp = (count lhs)
       1 (as-> lhs [x] {:x (parse-atom x)})
       2 (as-> lhs [f x] {:f f :x (parse-atom x)})
       3 (as-> lhs [x f y] {:f f :x (parse-atom x) :y (parse-atom y)}))]))

(def clamp (partial comp (partial bit-and 0xFFFF)))

(defn evaluate [k m]
  (let [eval-rec (fn [eval-rec-mem k]
                   (let [eval-rec (partial eval-rec-mem eval-rec-mem)]
                     (if (number? k)
                       k
                       (let [{:keys [f x y]} (m k)]
                         (case f
                           "NOT" ((clamp bit-not) (eval-rec x))
                           "AND" ((clamp bit-and) (eval-rec x) (eval-rec y))
                           "OR" ((clamp bit-or) (eval-rec x) (eval-rec y))
                           "LSHIFT" ((clamp bit-shift-left) (eval-rec x) (eval-rec y))
                           "RSHIFT" ((clamp bit-shift-right) (eval-rec x) (eval-rec y))
                           (eval-rec x))))))
        eval-rec-mem (memoize eval-rec)]
    (eval-rec-mem eval-rec-mem k)))

(defn p1 [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into {})
       (evaluate "a")))

(defn p2 [input]
  (let [m (->> input
               str/split-lines
               (map parse-line)
               (into {}))
        m (assoc m "b" {:x (evaluate "a" m)})]
    (evaluate "a" m)))