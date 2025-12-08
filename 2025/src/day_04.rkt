#lang racket

(define input (list->vector (string->list (apply string-append (file->lines "input/d4.txt")))))
;; (define input (list->vector (string->list (apply string-append (file->lines "input/d4_test.txt")))))

(define width (string-length (car (file->lines "input/d4.txt"))))
(define height (length (file->lines "input/d4.txt")))
;; (define width (string-length (car (file->lines "input/d4_test.txt"))))
;; (define height (length (file->lines "input/d4_test.txt")))

(define (grid-ref grid row col)
  (vector-ref grid (+ (* row height) col)))

(define (grid-set! grid row col v)
  (vector-set! grid (+ (* row height) col) v))

(define (forklift-accessible-p1? grid row col)
  (define adjacent-rolls
    (for*/sum ([i (in-inclusive-range (sub1 row) (add1 row))]
               [j (in-inclusive-range (sub1 col) (add1 col))]
               #:when (and (< -1 i height)
                           (< -1 j width)
                           (or (not (= i row)) (not (= j col)))
                           (char=? #\@ (grid-ref grid i j))))
      1))
  (< adjacent-rolls 4))

(define (forklift-accessible-p2? grid row col)
  (define adjacent-rolls
    (for*/sum ([i (in-inclusive-range (sub1 row) (add1 row))]
               [j (in-inclusive-range (sub1 col) (add1 col))]
               #:when (and (< -1 i height)
                           (< -1 j width)
                           (or (not (= i row)) (not (= j col)))
                           (char=? #\@ (grid-ref grid i j))))
      1))
  (< adjacent-rolls 4))

(define (forklift-accessible-gen lst)
  (match lst
    [(list sum grid)
     (define new-grid (vector-copy grid))
     (define new-rolls (for*/sum ([i (in-range height)]
                                  [j (in-range width)]
                                  #:when (and (char=? #\@ (grid-ref input i j))
                                              (forklift-accessible-p1? input i j)))
                         (grid-set! new-grid i j #\.)
                         1))
     (list (+ sum new-rolls) new-grid)]))

(println
 (for*/sum ([i (in-range height)]
            [j (in-range width)]
            #:when (and (char=? #\@ (grid-ref input i j))
                        (forklift-accessible-p1? input i j)))
   1))

;; TODO: Implement part 2 using mutation :(
