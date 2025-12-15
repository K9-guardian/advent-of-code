#lang racket

(require (rename-in data/integer-set
                    (count integer-set-count)))

(define input-string "input/d5.txt")

(define input (file->lines input-string))

(define (string->range str)
  (match (string-split str "-")
    [(list (app string->number start) (app string->number end))
     (make-range start end)]))

(match-define (list ranges-strings ... "" ingredients-string ...) input)
(define ranges (map string->range ranges-strings))
(define ingredients (map string->number ingredients-string))

(define (fresh? ingredient)
  (for/or ([r (in-list ranges)])
    (member? ingredient r)))

(count fresh? ingredients)

(define ingredient-set
  (for/fold ([fresh-ingredients (make-integer-set '())])
            ([r (in-list ranges)])
    (union fresh-ingredients r)))
(integer-set-count ingredient-set)
