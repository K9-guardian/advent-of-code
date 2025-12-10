#lang racket

(define input-string "input/d4.txt")

(define grid (list->vector (string->list (apply string-append (file->lines input-string)))))

(define width (string-length (car (file->lines input-string))))
(define height (length (file->lines input-string)))

(define (grid-ref grid row col)
  (vector-ref grid (+ (* row width) col)))

(define (grid-set! grid row col v)
  (vector-set! grid (+ (* row width) col) v))

(define (forklift-accessible? grid row col)
  (define adjacent-rolls
    (for*/sum ([i (in-inclusive-range (sub1 row) (add1 row))]
               [j (in-inclusive-range (sub1 col) (add1 col))]
               #:when (and (< -1 i height)
                           (< -1 j width)
                           (or (not (= i row)) (not (= j col)))
                           (char=? #\@ (grid-ref grid i j))))
      1))
  (< adjacent-rolls 4))

(for*/sum ([i (in-range height)]
           [j (in-range width)]
           #:when (and (char=? #\@ (grid-ref grid i j))
                       (forklift-accessible? grid i j)))
  1)

(define grid-copied (vector-copy grid))

(define (forklift-accessible-loop sum)
  (define forklift-accessible-rolls
    (for*/sum ([i (in-range height)]
               [j (in-range width)]
               #:when (and (char=? #\@ (grid-ref grid i j))
                           (forklift-accessible? grid i j)))
      (grid-set! grid-copied i j #\.)
      1))
  (vector-copy! grid 0 grid-copied)
  (if (zero? forklift-accessible-rolls)
      sum
      (forklift-accessible-loop (+ forklift-accessible-rolls sum))))

(forklift-accessible-loop 0)
