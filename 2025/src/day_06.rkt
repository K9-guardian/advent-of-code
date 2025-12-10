#lang racket

(define (transpose lst)
  (apply map list lst))

(define input-string "input/d6.txt")

(define input (map string-split (file->lines input-string)))

(for/sum ([problem (transpose input)])
  (match problem
    [(list nums-string ... "+")
     (apply + (map string->number nums-string))]
    [(list nums-string ... "*")
     (apply * (map string->number nums-string))]))

(define (all-spaces->commas lst)
  (if (andmap (curry char=? #\space) lst)
      (for/list ([_ (in-list lst)])
        #\,)
      lst))

(define problems (map (curryr string-split ",")
                      (map list->string
                           (transpose
                            (map all-spaces->commas
                                 (transpose
                                  (map string->list (file->lines "input/d6.txt"))))))))

(for/sum ([problem (in-list (transpose problems))])
  (match-define (list nums* ... op*) problem)
  (define op (match (string-trim op*) ["+" +] ["*" *]))
  (define nums (map
                (compose string->number (curryr string-replace " " "") list->string)
                (apply map list (map string->list nums*))))
  (apply op nums))
