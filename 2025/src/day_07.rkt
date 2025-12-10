#lang racket

(define input-file "input/d7.txt")

(define grid (list->vector (string->list (apply string-append (file->lines input-file)))))

(define width (string-length (car (file->lines input-file))))
(define height (length (file->lines input-file)))

(define (grid-ref grid row col)
  (vector-ref grid (+ (* row width) col)))

(define (grid-set! grid row col v)
  (vector-set! grid (+ (* row width) col) v))

(for*/sum ([row (in-range 1 height)]
           [col (in-range 0 width)]
           #:when (member (grid-ref grid (sub1 row) col) '(#\S #\|) char=?))
  (cond
    [(char=? #\^ (grid-ref grid row col))
     (grid-set! grid row (sub1 col) #\|)
     (grid-set! grid row (add1 col) #\|)
     1]
    [else
     (grid-set! grid row col #\|)
     0]))

(define memo (make-hash))
(define (count-timelines grid row col)
  (hash-ref!
   memo
   (cons row col)
   (λ ()
     (define junction-row
       (for/first ([i (in-range (add1 row) height)]
                   #:when (char=? #\^ (grid-ref grid i col)))
         i))
     (if junction-row
         (+ (count-timelines grid junction-row (sub1 col))
            (count-timelines grid junction-row (add1 col)))
         1))))

(count-timelines grid 0 (vector-member #\S grid char=?))
