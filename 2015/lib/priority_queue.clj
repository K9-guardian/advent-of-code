;; Persistent priority queue implementation using a binary heap. Min heap by default.
;; O(1) lookup, O(log n) insertion and deletion.

(ns priority-queue
  (:import clojure.lang.IPersistentCollection
           clojure.lang.IPersistentStack
           clojure.lang.Seqable))

(defn- left [idx] (inc (* 2 idx)))
(defn- right [idx] (+ 2 (* 2 idx)))
(defn- parent [idx] (quot (dec idx) 2))

(defn- swap-indices [arr idx1 idx2]
  (let [e1 (arr idx1) e2 (arr idx2)]
    (-> arr
        (assoc idx1 e2)
        (assoc idx2 e1))))

;; 1. Compare the added element with its parent; if they are in the correct order, stop.
;; 2. If not, swap the element with its parent and return to the previous step.
(defn- bubble-up [heap comparator]
  (loop [heap heap idx (dec (count heap))]
    (let [par-idx (parent idx)]
      (if (< (comparator (heap par-idx) (heap idx)) 0)
        heap
        (-> heap
            (swap-indices idx par-idx)
            (recur par-idx))))))

;; 1. Compare the new root with its children; if they are in the correct order, stop.
;; 2. If not, swap the element with one of its children and return to the previous step.
;;    (Swap with its smaller child in a min-heap and its larger child in a max-heap.)
(defn- bubble-down [heap comparator]
  (letfn [(min* [idx1 idx2] ; Accounts for if second idx is out of bounds
            (if (and (< idx2 (count heap))
                     (< (comparator (heap idx2) (heap idx1)) 0))
              idx2
              idx1))]
    (loop [heap heap idx 0]
      (let [left-idx (left idx)
            right-idx (right idx)
            largest (-> idx (min* left-idx) (min* right-idx))]
        (if (= largest idx)
          heap
          (-> heap
              (swap-indices idx largest)
              (recur largest)))))))

(deftype PersistentPriorityQueue [heap comparator]
  Seqable
  (seq [_] (seq heap))

  IPersistentCollection
  (cons [_ [element priority]]
    (-> heap
        (conj [element priority])
        (bubble-up comparator)
        (PersistentPriorityQueue. comparator)))
  (count [_] (count heap))
  (empty [_] (PersistentPriorityQueue. comparator (empty heap)))
  (equiv [this other] (= (.seq this) (seq other)))

  IPersistentStack
  (peek [_] (first heap))
  (pop [_]
    (if-not (pos? (count heap))
      (throw (IllegalStateException. "Can't pop empty priority queue"))
      (-> heap
          (assoc 0 (peek heap))
          pop
          (bubble-down comparator)
          (PersistentPriorityQueue. comparator)))))

;; Makes a comparator that only looks at the priority of a pair.
(defn- priority-comparator [comparator]
  (fn [[_ p1] [_ p2]]
    (comparator p1 p2)))

;; Keys are elements, vals are priority.
(defn priority-queue [& keyvals]
  {:pre [(even? (count keyvals))]}
  (into (PersistentPriorityQueue. [] (priority-comparator compare)) (partition 2 keyvals)))

(defn priority-queue-by [comparator & keyvals]
  {:pre [(even? (count keyvals))]}
  (into (PersistentPriorityQueue. [] (priority-comparator comparator)) (partition 2 keyvals)))