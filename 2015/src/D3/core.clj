(ns D3.core)

(def input (slurp "input/d3.txt"))

(defn update-loc [loc mov]
  (-> loc
      (update 0 (case mov \> inc \< dec identity))
      (update 1 (case mov \^ inc \v dec identity))))

(defn p1 [input]
  (->> input
       (reduce (fn [{:keys [loc visited]} mov]
                 (let [new-loc (update-loc loc mov)]
                   {:loc new-loc :visited (conj visited new-loc)}))
               {:loc [0 0] :visited #{[0 0]}})
       :visited
       count))

(defn p2 [input]
  (->> input
       (partition 2)
       (reduce (fn [{:keys [santa-loc robo-loc visited]} [mov1 mov2]]
                 (let [new-santa-loc (update-loc santa-loc mov1)
                       new-robo-loc (update-loc robo-loc mov2)]
                   {:santa-loc new-santa-loc
                    :robo-loc new-robo-loc
                    :visited (conj visited new-santa-loc new-robo-loc)}))
               {:santa-loc [0 0] :robo-loc [0 0] :visited #{[0 0]}})
       :visited
       count))