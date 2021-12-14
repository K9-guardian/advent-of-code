(def input (slurp "d3.txt"))

(defn d1 [input]
  (->>
   input
   (reduce
    (fn [[[x y] st] v]
      (let [x* ((case v \> inc \< dec identity) x)
            y* ((case v \^ inc \v dec identity) y)]
        (prn [x* y*])
        [[x* y*] (conj! st [x* y*])]))
    [[0 0] (transient #{[0 0]})])
   second
   persistent!
   count))

(defn d2 [input]
  (->>
   input
   (partition 2)
   (reduce
    (fn [[[x1 y1] [x2 y2] st] [v1 v2]]
      (let [x1* ((case v1 \> inc \< dec identity) x1)
            y1* ((case v1 \^ inc \v dec identity) y1)
            x2* ((case v2 \> inc \< dec identity) x2)
            y2* ((case v2 \^ inc \v dec identity) y2)]
        (conj! st [x1* y1*])
        (conj! st [x2* y2*])
        [[x1* y1*]
         [x2* y2*]
         st]))
    [[0 0] [0 0] (transient #{[0 0]})])
   peek
   persistent!
   count))