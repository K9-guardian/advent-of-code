#lang racket

(require racket/list/grouping)

(define input-string "input/d2.txt")

(define input (string-split (car (file->lines input-string)) ","))

(define (invalid-id-p1? id)
  (and (even? (string-length id))
       (string=? (substring id 0 (/ (string-length id) 2))
                 (substring id (/ (string-length id) 2)))))

(define (invalid-id-p2? id)
  (for/first ([i (in-inclusive-range 1 (quotient (string-length id) 2))]
              #:when (zero? (modulo (string-length id) i))
              #:when (let ([partitions (windows i i (string->list id))])
                       (andmap (λ (v) (equal? (car partitions) v)) (cdr partitions))))
    id))

(define (invalid-ids-p1 start end)
  (filter invalid-id-p1? (map number->string (inclusive-range start end))))

(define (invalid-ids-p2 start end)
  (filter invalid-id-p2? (map number->string (inclusive-range start end))))

(apply +
       (map string->number
            (flatten
             (for/list ([interval (in-list input)])
               (match (string-split interval "-")
                 [(list (app string->number start) (app string->number end))
                  (invalid-ids-p1 start end)])))))

(apply +
       (map string->number
            (flatten
             (for/list ([interval (in-list input)])
               (match (string-split interval "-")
                 [(list (app string->number start) (app string->number end))
                  (invalid-ids-p2 start end)])))))
