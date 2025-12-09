#lang racket

(require data/interval-map)

(define input (file->lines "input/d5.txt"))

(define (string->range s)
  (match (string-split s "-")
    [(list (app string->number start) (app string->number end))
     (cons start end)]))

(match-define (list ranges* ... "" ingredients* ...) input)
(define ranges (map string->range ranges*))
(define ingredients (map string->number ingredients*))

(define (fresh? ingredient)
  (for/or ([r (in-list ranges)])
    (<= (car r) ingredient (cdr r))))

(println (count fresh? ingredients))

(define m (make-interval-map))
(for ([r (in-list ranges)])
  (interval-map-set! m (car r) (add1 (cdr r)) #t))
(println
 (for/sum ([(r _) (in-dict m)])
   (- (cdr r) (car r))))
