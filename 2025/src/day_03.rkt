#lang racket

(define (numeric-char->integer char)
  (- (char->integer char) (char->integer #\0)))

(define input-string "input/d3.txt")

(define input
  (for/list ([str (in-list (file->lines input-string))])
    (list->vector (map numeric-char->integer (string->list str)))))

(define (largest-voltage-p1 bank)
  (define second-voltage (sub1 (vector-length bank)))
  (define first-voltage (sub1 (sub1 (vector-length bank))))
  (for ([i (in-inclusive-range first-voltage 0 -1)])
    (when (>= (vector-ref bank i) (vector-ref bank first-voltage))
      (set! first-voltage i)))
  (for ([i (in-range second-voltage first-voltage -1)])
    (when (>= (vector-ref bank i) (vector-ref bank second-voltage))
      (set! second-voltage i)))
  (+ (* 10 (vector-ref bank first-voltage)) (vector-ref bank second-voltage)))

(define (largest-voltage-p2 bank)
  (define voltages (for/vector ([i (in-range 12)])
                     (+ i (- (vector-length bank) 12))))
  (define lower-bound -1)
  (for ([i (in-range (vector-length voltages))])
    (for ([j (in-range (vector-ref voltages i) lower-bound -1)])
      (when (>= (vector-ref bank j) (vector-ref bank (vector-ref voltages i)))
        (vector-set! voltages i j)))
    (set! lower-bound (vector-ref voltages i)))
  (string->number
   (apply string-append
          (map number->string
               (vector->list
                (vector-map (λ (i) (vector-ref bank i)) voltages))))))

(apply + (map largest-voltage-p1 input))

(apply + (map largest-voltage-p2 input))
