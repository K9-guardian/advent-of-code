(ns day-03)

(def input (slurp "input/d3.txt"))

(def move->vec
  {\^ [0 1]
   \v [0 -1]
   \> [1 0]
   \< [-1 0]})

(defn p1 [input]
  (->> input
       (reduce (fn [{:keys [loc visited]} mov]
                 (let [loc (mapv + loc (move->vec mov))]
                   {:loc loc :visited (conj visited loc)}))
               {:loc [0 0] :visited #{[0 0]}})
       :visited
       count))

(defn p2 [input]
  (->> input
       (partition 2)
       (reduce (fn [{:keys [santa-loc robo-loc visited]} [santa-mov robo-mov]]
                 (let [santa-loc (mapv + santa-loc (move->vec santa-mov)) 
                       robo-loc (mapv + robo-loc (move->vec robo-mov))]
                   {:santa-loc santa-loc
                    :robo-loc robo-loc
                    :visited (conj visited santa-loc robo-loc)}))
               {:santa-loc [0 0] :robo-loc [0 0] :visited #{[0 0]}})
       :visited
       count))