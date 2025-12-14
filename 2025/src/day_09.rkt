#lang racket

;; Adapted the solution from https://todd.ginsberg.com/post/advent-of-code/2025/day9/.
;; I had figured out that the region didn't have any holes and thus all you had to check
;; was that the boundary was inside the region on my own. However, I did not figure out the next
;; step of using the intersection of the boundary lines with the inset rectangles to determine
;; if the rectangle boundaries are within the region.

(require data/integer-set)

(define input-string "input/d9.txt")

(define (list->coordinate lst)
  (cons (first lst) (second lst)))

(define input
  (list->vector
   (map (compose list->coordinate
                 (λ (str) (map string->number str))
                 (λ (str) (string-split str ",")))
        (file->lines input-string))))

;; Rectangles are a pair of ranges representing the x and y travel.
(define (rectangle p q)
  (cons (make-range (min (car p) (car q)) (max (car p) (car q)))
        (make-range (min (cdr p) (cdr q)) (max (cdr p) (cdr q)))))

(define (area rect)
  (* (count (car rect)) (count (cdr rect))))

(for*/fold ([max-area -inf.0])
           ([i (in-range (vector-length input))]
            [j (in-range (add1 i) (vector-length input))])
  (define p (vector-ref input i))
  (define q (vector-ref input j))
  (max (area (rectangle p q)) max-area))

;; For part 2, we use the fact that our region does not contain any holes.
;; This means if the boundary of a rectangle is within the region,
;; then the entire rectangle must be within the region.
;; This can alternatively be stated as checking if any boundary line intersects with our rectangle.

;; Note that because our rectangle can be part of the boundary,
;; we need to shrink our rectangles by 1 in all directions before applying the check.

;; First, we construct all boundary lines as a list of 1 width rectangles.
(define boundary-lines
  (for/list ([i (in-range (vector-length input))]
             [j (in-range 1 (add1 (vector-length input)))])
    (define p (vector-ref input i))
    (define q (vector-ref input (modulo j (vector-length input)))) ; wraparound trick
    (rectangle p q)))

;; Now we define an overlaps? function using intersect.
(define (overlaps? rect0 rect1)
  (and (positive? (count (intersect (car rect0) (car rect1))))
       (positive? (count (intersect (cdr rect0) (cdr rect1))))))

;; Construct a rectangle that's inset by 1
(define (inset rect)
  (match-define (cons p0 q0) (first (integer-set-contents (car rect))))
  (match-define (cons p1 q1) (first (integer-set-contents (cdr rect))))
  (cond
    [(and (= p0 q0) (= p1 q1)) (cons (make-range) (make-range))]
    [(= p0 q0) (cons (make-range) (make-range (add1 p1) (sub1 q1)))]
    [(= p1 q1) (cons (make-range (add1 p0) (sub1 q0)) (make-range))]
    [else (cons (make-range (add1 p0) (sub1 q0))
                (make-range (add1 p1) (sub1 q1)))]))

;; Now we check if the inset rectangle overlaps with any of the boundary lines.
(for*/fold ([max-area -inf.0])
           ([i (in-range (vector-length input))]
            [j (in-range (+ i 1) (vector-length input))])
  (define p (vector-ref input i))
  (define q (vector-ref input j))
  (define rect (rectangle p q))
  (if (ormap (λ (line) (overlaps? (inset rect) line)) boundary-lines)
      max-area
      (max (area rect) max-area)))
