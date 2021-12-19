(ns D12.core
  (:require [cheshire.core :refer :all]
            [clojure.walk :as w]))

(def input (parse-string (slurp "input/d12.txt")))

(defn p1 [input]
  (let [x (atom 0)]
    (w/postwalk #(when (number? %) (swap! x + %)) input)
    @x))

(defn p2 [input]
  (let [x (atom 0)]
    (->>
     input
     (w/postwalk #(if (and (map? %) (->> % vals (some #{"red"}))) nil %))
     (w/postwalk #(when (number? %) (swap! x + %))))
    @x))