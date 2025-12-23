#lang racket

(define input-string "input/d11.txt")

(define input
  (for/hash ([str (in-list (file->lines input-string))])
    (match-define (cons key vals) (string-split str))
    (values (substring key 0 (sub1 (string-length key))) (list->set vals))))

(define memo (make-hash))
(define (dfs node goal)
  (hash-ref!
   memo
   (cons node goal)
   (λ ()
     (cond
       [(string=? goal node) 1]
       [else
        (for/sum ([adjacent-node (in-set (hash-ref input node (set)))])
          (dfs adjacent-node goal))]))))

(dfs "you" "out")

(+ (* (dfs "svr" "fft")
      (dfs "fft" "dac")
      (dfs "dac" "out"))
   (* (dfs "svr" "dac")
      (dfs "dac" "fft")
      (dfs "fft" "out")))
