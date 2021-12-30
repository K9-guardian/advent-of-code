;; Persistent priority queue implementation using a bucketed sorted map. Min heap by default.
;; O(1) lookup. O(log n) insertion and deletion.

(ns priority-queue
  (:import clojure.lang.IPersistentCollection
           clojure.lang.IPersistentStack
           clojure.lang.Seqable))

(deftype PersistentPriorityQueue [num-elements buckets]
  Seqable
  (seq [_] (->> buckets
                (map (fn [[priority values]] (map #(clojure.lang.MapEntry. priority %) values)))
                (apply concat)))

  IPersistentCollection
  (cons [_ [priority value]]
    (PersistentPriorityQueue. (inc num-elements)
                              (update buckets
                                      priority
                                      (fnil conj [])
                                      value)))
  (count [_] num-elements)
  (empty [_] (PersistentPriorityQueue. 0 (empty buckets)))
  (equiv [this other] (= (.seq this) (seq other)))

  IPersistentStack
  (peek [this] (first (.seq this)))
  (pop [_]
    (if (empty? buckets)
      (throw (IllegalStateException. "Can't pop empty priority queue"))
      (let [top (-> buckets first key)
            buckets (update buckets top pop)]
        (PersistentPriorityQueue.
         (dec num-elements)
         (if (empty? (buckets top))
           (dissoc buckets top)
           buckets))))))

(defn priority-queue [& keyvals]
  (let [partitions (partition 2 keyvals)]
    (if (odd? (count keyvals))
      (throw (IllegalArgumentException. (str "No value supplied for priority: " (last keyvals))))
      (into (PersistentPriorityQueue. 0 (sorted-map)) partitions))))

(defn priority-queue-by [comparator & keyvals]
  (let [partitions (partition 2 keyvals)]
    (if (odd? (count keyvals))
      (throw (IllegalArgumentException. (str "No value supplied for priority: " (last keyvals))))
      (into (PersistentPriorityQueue. 0 (sorted-map-by comparator)) partitions))))