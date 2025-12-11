(ns day-04
  (:import AdventCoinMiner))

(def input (slurp "input/d4.txt"))

(defn p1 [input]
  (.findNonce (AdventCoinMiner. input 5)))

(defn p2 [input]
  (.findNonce (AdventCoinMiner. input 6)))

(comment
  (p1 input)
  (p2 input))
