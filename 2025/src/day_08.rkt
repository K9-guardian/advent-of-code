#lang racket

(require data/heap)
(require data/union-find)

(define input-file "input/d8.txt")

(define (string->coordinate s)
  (map string->number (string-split s ",")))

(define input (list->vector (map string->coordinate (file->lines input-file))))

(define (distance coordinate0 coordinate1)
  (+ (sqr (- (car coordinate0) (car coordinate1)))
     (sqr (- (cadr coordinate0) (cadr coordinate1)))
     (sqr (- (caddr coordinate0) (caddr coordinate1)))))

(define distance-heap (make-heap <=))
(define distance->edges (make-hash))
(for* ([i (in-range (vector-length input))]
       [j (in-range (add1 i) (vector-length input))])
  (define dist (distance (vector-ref input i) (vector-ref input j)))
  (heap-add! distance-heap dist)
  (hash-set! distance->edges dist (cons i j)))

(define ufs
  (for*/vector ([i (in-range (vector-length input))])
    (uf-new i)))

(define distance-heap* (heap-copy distance-heap))
(define ufs* (vector-copy ufs))

(for ([dist (in-heap/consume! distance-heap)]
      [_ (in-range 1000)])
  (define edge (hash-ref distance->edges dist))
  (uf-union! (vector-ref ufs (car edge)) (vector-ref ufs (cdr edge))))

(define counter (make-hash))

(for ([v (in-list (map uf-find (vector->list ufs)))])
  (if (hash-has-key? counter v)
      (hash-update! counter v add1)
      (hash-set! counter v 1)))

(apply * (take (sort (hash-values counter) >) 3))

(define last-edge #f)
(for ([dist (in-heap/consume! distance-heap*)]
      #:when (let ([edge (hash-ref distance->edges dist)])
               (not
                (uf-same-set? (vector-ref ufs* (car edge))
                              (vector-ref ufs* (cdr edge))))))
  (define edge (hash-ref distance->edges dist))
  (uf-union! (vector-ref ufs* (car edge)) (vector-ref ufs* (cdr edge)))
  (set! last-edge edge))
(* (car (vector-ref input (car last-edge)))
   (car (vector-ref input (cdr last-edge))))
