#lang racket

(require racket/list/iteration)

(define input (file->lines "input/d1.txt"))

(define (turn-dial instruction position)
  (match (regexp-match #px"([LR])(\\d+)" instruction)
    [(list _ "L" (app string->number amount)) (modulo (- position amount) 100)]
    [(list _ "R" (app string->number amount)) (modulo (+ position amount) 100)]))

(define (num-zeros instruction position)
  (match (regexp-match #px"([LR])(\\d+)" instruction)
    [(list _ "L" (app string->number amount))
     (define new-position (modulo (- position amount) 100))
     (+ (quotient amount 100)
        (if (zero? new-position) 1 0)
        (if (and (not (zero? position)) (< position new-position)) 1 0))]
    [(list _ "R" (app string->number amount))
     (define new-position (modulo (+ position amount) 100))
     (+ (quotient amount 100) (if (> position new-position) 1 0))]))

(displayln (count zero? (running-foldl turn-dial 50 input)))
(define-values (zeros _)
  (for/fold ([zeros 0] [position 50])
            ([instruction (in-list input)])
    (values (+ (num-zeros instruction position) zeros) (turn-dial instruction position))))
(displayln zeros)
