#lang racket

(require (rename-in data/integer-set
                    (count integer-set:count)))

(define input-file "input/d5.txt")

(define input (file->lines input-file))

(define (string->range s)
  (match (string-split s "-")
    [(list (app string->number start) (app string->number end))
     (make-range start end)]))

(match-define (list ranges* ... "" ingredients* ...) input)
(define ranges (map string->range ranges*))
(define ingredients (map string->number ingredients*))

(define (fresh? ingredient)
  (for/or ([r (in-list ranges)])
    (member? ingredient r)))

(count fresh? ingredients)

(define ingredient-set (foldl union (make-integer-set '()) ranges))
(integer-set:count ingredient-set)
