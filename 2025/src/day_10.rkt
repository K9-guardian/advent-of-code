#lang racket

(require data/queue)
(require glpk)

(define input-string "input/d10.txt")

(define input (for/list ([str (in-list (file->lines input-string))])
                (match (string-split str)
                  [(list lights-string buttons-string ... joltage-string)
                   (define lights
                     (map (match-λ [#\# #t] [#\. #f])
                          (string->list (string-trim lights-string #px"[\\[\\]]"))))
                   (define buttons
                     (for/list ([button-string (in-list buttons-string)])
                       (map string->number
                            (string-split (string-trim button-string #px"[\\(\\)]") ","))))
                   (define joltage
                     (map string->number
                          (string-split (string-trim joltage-string #px"[\\{\\}]") ",")))
                   (list lights buttons joltage)])))

(define (lights-update lights button)
  (for/fold ([lights lights])
            ([idx (in-list button)])
    (list-update lights idx not)))

(define (lights-bfs lights buttons)
  (define root (make-list (length lights) #f))
  (define queue (make-queue))
  (enqueue! queue root)
  (define explored-nodes (make-hash))
  (hash-set! explored-nodes root #f)
  (let loop ()
    (define node (dequeue! queue))
    (cond
      [(equal? node lights)
       (let loop ([node node] [dist 0])
         (define parent (hash-ref explored-nodes node))
         (if parent
             (loop (hash-ref explored-nodes node) (add1 dist))
             dist))]
      [else
       (for ([edge (in-list buttons)])
         (define adjacent-node (lights-update node edge))
         (when (not (hash-has-key? explored-nodes adjacent-node))
           (hash-set! explored-nodes adjacent-node node)
           (enqueue! queue adjacent-node)))
       (loop)])))

(define (joltage-lp joltage buttons)
  (define buttons-symbols
    (for/list ([i (in-range (length buttons))])
      (string->symbol (string-append "b" (number->string i)))))
  (define joltage-symbols
    (for/list ([i (in-range (length joltage))])
      (string->symbol (string-append "j" (number->string i)))))
  (define objective (cons 0 (map (λ (b) (list 1 b)) buttons-symbols)))
  (define constraints
    (for/list ([(joltage-symbol i) (in-indexed joltage-symbols)])
      (cons joltage-symbol
            (for/list ([button (in-list buttons)]
                       [button-symbol (in-list buttons-symbols)]
                       #:when (member i button))
              (list 1 button-symbol)))))
  (define bounds
    (append (map (λ (j n) (list j n n)) joltage-symbols joltage)
            (map (λ (b) (list b 0 'posinf)) buttons-symbols)))
  (match (mip-solve objective 'min constraints bounds buttons-symbols)
    [(list 'good #f (cons presses _)) presses]))

(for/sum ([machine (in-list input)])
  (match machine
    [(list lights buttons _) (lights-bfs lights buttons)]))

(for/sum ([machine (in-list input)])
  (match machine
    [(list _ buttons joltage) (joltage-lp joltage buttons)]))
