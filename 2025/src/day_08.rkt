#lang racket

(require data/heap)
(require data/union-find)

(define input-string "input/d8.txt")

(define (string->coordinate s)
  (map string->number (string-split s ",")))

(define input (list->vector (map string->coordinate (file->lines input-string))))

(define (distance coordinate0 coordinate1)
  (match/values
   (values coordinate0 coordinate1)
   [((list x0 y0 z0) (list x1 y1 z1))
    (+ (sqr (- x1 x0))
       (sqr (- y1 y0))
       (sqr (- z1 z0)))]))

(define distance-heap (make-heap <=))
(define distance-edge-hash (make-hash))
(for* ([i (in-range (vector-length input))]
       [j (in-range (add1 i) (vector-length input))])
  (define dist (distance (vector-ref input i)
                         (vector-ref input j)))
  (heap-add! distance-heap dist)
  (hash-set! distance-edge-hash dist (cons i j)))

(define ufs
  (for*/vector ([i (in-range (vector-length input))])
    (uf-new i)))

;; Used for part 2 - copying them early before they get mutated
(define distance-heap-copied (heap-copy distance-heap))
(define ufs-copied (vector-copy ufs))

(for ([dist (in-heap/consume! distance-heap)]
      [_ (in-range 1000)])
  (define edge (hash-ref distance-edge-hash dist))
  (uf-union! (vector-ref ufs (car edge))
             (vector-ref ufs (cdr edge))))

(define counter (make-hash))

(for ([v (in-list (map uf-find (vector->list ufs)))])
  (if (hash-has-key? counter v)
      (hash-update! counter v add1)
      (hash-set! counter v 1)))

(apply * (take (sort (hash-values counter) >) 3))

(define last-edge
  (for/fold ([last-edge #f])
            ([dist (in-heap/consume! distance-heap-copied)])
    (define edge (hash-ref distance-edge-hash dist))
    (cond
      [(not (uf-same-set? (vector-ref ufs-copied (car edge))
                          (vector-ref ufs-copied (cdr edge))))
       (uf-union! (vector-ref ufs-copied (car edge))
                  (vector-ref ufs-copied (cdr edge)))
       edge]
      [else last-edge])))
(* (car (vector-ref input (car last-edge)))
   (car (vector-ref input (cdr last-edge))))
