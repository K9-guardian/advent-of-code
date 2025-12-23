#lang racket

(define input-string "input/d12.txt")

(define (string->region str)
  (match-define (list _ width height) (regexp-match #px"(\\d+)x(\\d+)" str))
  (cons (string->number height) (string->number width)))

(define input
  (for/list ([str (in-list (file->lines input-string))]
             #:when (regexp-match #px"\\d+x\\d+:( \\d+)+" str))
    (match-define (cons region-string shape-counts-strings) (string-split str))
    (cons (string->region region-string)
          (map string->number shape-counts-strings))))

(for/sum ([problem (in-list input)])
  (match-define (cons region shape-counts) problem)
  (define area (* (car region) (cdr region)))
  (if (<= (* 9 (apply + shape-counts)) area) 1 0))
