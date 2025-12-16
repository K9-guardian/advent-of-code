#lang racket

(define input-string "input/d9.txt")

(define (list->point lst)
  (cons (first lst) (second lst)))

(define (point-add p q)
  (cons (+ (car p) (car q)) (+ (cdr p) (cdr q))))

(define input
  (for/vector ([str (in-list (file->lines input-string))])
    (list->point (map string->number (string-split str ",")))))

;; Before we begin we want to compress these coordinates
(define xs-compressed
  (vector-sort
   (for/vector ([xs (in-slice 2 (vector-map car input))])
     (first xs))
   <))
(define ys-compressed
  (vector-sort
   (for/vector ([ys (in-slice 2 (vector-map cdr input))])
     (first ys))
   <))

(define (x-compressed->expanded i) (vector-ref xs-compressed i))
(define (y-compressed->expanded j) (vector-ref ys-compressed j))

(define input-compressed
  (for/vector ([coordinate (in-vector input)])
    (match coordinate
      [(cons x y) (cons (vector-member x xs-compressed)
                        (vector-member y ys-compressed))])))

;; We construct a rectangle as a normalized pair of compressed coordinates.
(define (rectangle p q)
  (define xmin (min (car p) (car q)))
  (define xmax (max (car p) (car q)))
  (define ymin (min (cdr p) (cdr q)))
  (define ymax (max (cdr p) (cdr q)))
  (cons (cons xmin ymin) (cons xmax ymax)))

;; We calculate area using expanded coordinates
(define (area rect)
  (match rect
    [(cons (cons (app x-compressed->expanded xmin) (app y-compressed->expanded ymin))
           (cons (app x-compressed->expanded xmax) (app y-compressed->expanded ymax)))
     (* (add1 (- xmax xmin))
        (add1 (- ymax ymin)))]))

;; For part 1, iterate over all rectangles and find the maximum area
(for*/fold ([max-area -inf.0])
           ([i (in-range (vector-length input-compressed))]
            [j (in-range (add1 i) (vector-length input-compressed))])
  (define p (vector-ref input-compressed i))
  (define q (vector-ref input-compressed j))
  (max (area (rectangle p q)) max-area))

;; For part 2, we find all the points within our region and use this region
;; to check if a rectangle is within bounds. This runs fast because we compressed coordinates.

;; We start by gathering all the points in our boundary.
(define region-points (mutable-set))
(for ([i (in-range (vector-length input-compressed))]
      [j (in-range 1 (add1 (vector-length input-compressed)))])
  (define p (vector-ref input-compressed i))
  (define q (vector-ref input-compressed (modulo j (vector-length input-compressed)))) ; wraparound trick
  (match-define (cons (cons xmin ymin) (cons xmax ymax)) (rectangle p q))
  (define boundary-points
    (cond
      [(= xmin xmax) (map (λ (y) (cons xmin y)) (inclusive-range ymin ymax))]
      [(= ymin ymax) (map (λ (x) (cons x ymin)) (inclusive-range xmin xmax))]))
  (for ([p (in-list boundary-points)])
    (set-add! region-points p)))

;; Now we add all the points inside our region We do this by first finding a single point
;; within our region and then flood filling with a standard DFS.

;; We find a point within our region by randomly selecting a point and casting rays in all
;; 4 directions. If all the rays cross the boundary an odd number of times, this point is
;; inside the region.
(define (within-bounding-box? point)
  (and (< -1 (car point) (vector-length xs-compressed))
       (< -1 (cdr point) (vector-length ys-compressed))))

(define (raycast point direction)
  (let loop ([point point]
             [ray (list point)])
    (if (within-bounding-box? point)
        (loop (point-add point direction) (cons point ray))
        ray)))

(define root
  (let loop ([point (cons (random (vector-length xs-compressed))
                          (random (vector-length ys-compressed)))])
    (if (for/and ([direction '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))])
          (odd? (count (λ (p) (set-member? region-points p)) (raycast point direction))))
        point
        (loop (cons (random (vector-length xs-compressed)) (random (vector-length ys-compressed)))))))

;; Now we flood fill from root and populate region-points.
(let loop ([point root])
  (set-add! region-points point)
  (for ([direction '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))])
    (define adjacent-point (point-add point direction))
    (unless (set-member? region-points adjacent-point)
      (loop adjacent-point))))

;; Finally, we need a function to get the boundary points of a rectangle.
(define (rectangle->boundary rect)
  (match-define (cons (cons xmin ymin) (cons xmax ymax)) rect)
  (append (map (λ (x) (cons x ymin)) (inclusive-range xmin xmax))
          (map (λ (x) (cons x ymax)) (inclusive-range xmin xmax))
          (map (λ (y) (cons xmin y)) (inclusive-range ymin ymax))
          (map (λ (y) (cons xmax y)) (inclusive-range ymin ymax))))

;; Now, we repeat our loop for part 1 but add the additional check that our rectangle
;; boundary is within the region.
(for*/fold ([max-area -inf.0])
           ([i (in-range (vector-length input-compressed))]
            [j (in-range (add1 i) (vector-length input-compressed))])
  (define p (vector-ref input-compressed i))
  (define q (vector-ref input-compressed j))
  (if (for/and ([p (in-list (rectangle->boundary (rectangle p q)))])
        (set-member? region-points p))
      (max (area (rectangle p q)) max-area)
      max-area))
