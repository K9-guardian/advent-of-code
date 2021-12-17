(def input (slurp "d9.txt"))
(require '[clojure.string :as str])

(defn parse-string [s]
  (->>
   s
   (re-seq #"(\w+) to (\w+) = (\d+)")
   nfirst))

;; From SICP
(defn permutations [coll]
  (if (empty? coll)
    [[]]
    (mapcat
     #(map
       (partial cons %)
       (permutations (remove #{%} coll)))
     coll)))

(defn path->distance [m p]
  (reduce
   (fn [d [start end]]
     (+ d (get-in m [start end])))
   0
   (partition 2 1 p)))

(defn p1 [input]
  (as-> input $
    (str/split-lines $)
    (map parse-string $)
    (reduce
     (fn [m [start end d]]
       (->
        m
        (update (keyword start) (fnil conj {}) [(keyword end) (Integer/parseInt d)])
        (update (keyword end) (fnil conj {}) [(keyword start) (Integer/parseInt d)])))
     {}
     $)
    (let [paths (-> $ keys permutations)]
      (->>
       paths
       (map (partial path->distance $))
       (apply min)))))

(defn p2 [input]
  (as-> input $
    (str/split-lines $)
    (map parse-string $)
    (reduce
     (fn [m [start end d]]
       (->
        m
        (update (keyword start) (fnil conj {}) [(keyword end) (Integer/parseInt d)])
        (update (keyword end) (fnil conj {}) [(keyword start) (Integer/parseInt d)])))
     {}
     $)
    (let [paths (-> $ keys permutations)]
      (->>
       paths
       (map (partial path->distance $))
       (apply max)))))