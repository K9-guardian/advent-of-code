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

(defn prog->graph [prog]
  (->> prog
       (mapcat (fn [[v {x :x y :y}]]
                  (->> [x y]
                       (remove nil?)
                       (map #(hash-map % #{v}))
                       (cons {v #{}})))) ; Ensures that every node is a key
       (apply merge-with into)))

;; https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
(defn topsort [g]
  (let [l (atom '())
        marks (atom (zipmap (keys g) (repeat :none)))
        visit (fn visit [n]
                (case (@marks n)
                  :perm nil
                  :temp (throw (IllegalArgumentException. "Not a DAG"))
                  :none (do
                          (swap! marks assoc n :temp)
                          (run! visit (g n))
                          (swap! marks assoc n :perm)
                          (swap! l conj n))))]
    (while (->> @marks vals (some #{:none :temp}))
      (let [n (->> @marks (filter (comp #{:none} val)) ffirst)]
        (visit n)))
    @l))

(->> input str/split-lines (map parse-line) prog->graph topsort (drop-while number?))

(def clamp (partial comp (partial bit-and 0xFFFF)))

(defn evaluate [k prog]
  (loop [env {} ts (->> prog prog->graph topsort)]
    (if-let [[t & ts] (seq ts)]
      (if (number? t)
        (recur (assoc env t t) ts)
        (let [{:keys [f x y]} (prog t)]
          (recur (assoc env
                        t
                        (case f
                          "NOT" ((clamp bit-not) (env x))
                          "AND" ((clamp bit-and) (env x) (env y))
                          "OR" ((clamp bit-or) (env x) (env y))
                          "LSHIFT" ((clamp bit-shift-left) (env x) (env y))
                          "RSHIFT" ((clamp bit-shift-right) (env x) (env y))
                          (env x)))
                 ts)))
      (env k))))

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