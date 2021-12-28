(ns D23.core
  (:require [clojure.string :as str]))

(def input (slurp "input/d23.txt"))

;; instruction ::= ("hlf" | "tpl" | "inc") reg | "jmp" num | ("jie" | "jio") reg "," num

;; reg ::= "a" | "b"
;; num ::= \d+

(defn parse-line [l]
  (let [[instr & more] (str/split l #",? ")]
    (case instr
      ("hlf" "tpl" "inc") (as-> more [reg] {:instr (keyword instr) :reg (keyword reg)})
      ("jie" "jio") (as-> more [reg offset]
                      {:instr (keyword instr) :reg (keyword reg) :offset (Integer/parseInt offset)})
      "jmp" (as-> more [offset] {:instr (keyword instr) :offset (Integer/parseInt offset)}))))

(defn solve [rs tape]
  (letfn [(solve-iter [rs i]
            (if-not (< -1 i (count tape))
              (rs :b)
              (let [{:keys [instr reg offset]} (tape i)]
                (case instr
                  :hlf (recur (update rs reg #(quot % 2)) (inc i))
                  :tpl (recur (update rs reg (partial * 3)) (inc i))
                  :inc (recur (update rs reg inc) (inc i))
                  :jmp (recur rs (+ i offset))
                  :jie (recur rs (+ i (if (even? (reg rs)) offset 1)))
                  :jio (recur rs (+ i (if (= 1 (reg rs)) offset 1)))))))]
    (solve-iter rs 0)))

(defn p1 [input]
  (->> input
       str/split-lines
       (mapv parse-line)
       (solve {:a 0 :b 0})))

(defn p2 [input]
  (->> input
       str/split-lines
       (mapv parse-line)
       (solve {:a 1 :b 0})))