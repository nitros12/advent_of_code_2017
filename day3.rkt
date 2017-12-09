#lang racket

;; maximum value is always in the bottom right of a 'ring' which is of value of the ring width squared
;; if we get a value, we then know that the ceiling (2n+1)^2 number is it's ring index.
;; then we can split into a 4 condition depending on which section of the spiral we're on.

(define (get-row n)
  (add1 (floor (/ (sub1 (integer-sqrt (sub1 n)))
                  2))))

(define (get-base row)
  (expt (add1 (* row
                 2))
        2))

(define (get-quadrant n)
  (let* ([row (get-row n)]
         [base (get-base row)]
         [prev (get-base (sub1 row))])
    (floor (* 4
              (/ (- n prev)
                 (- base prev))))))

(define (solution-1 n)
  ; The offset is determined by the quadrant we're in
  ; if we're in the first quadrant, then the offset is our row index + ceiling elevation from row (2n + 1) / 2
  ; if we're in the second, offset is again row index and elevation is again but - quadrant
  (if (= n 1)
      0 ; special case because I'm lazy
      (let* ([quad (get-quadrant n)]
             [row (get-row n)]
             [base (get-base row)]
             [prev (get-base (sub1 row))]
             [offset (- n prev)])
        (let ([diff (- offset (* quad 2 row))])
          (+ row (abs (- row diff)))))))


(define (sumn hash x y)
  (for/sum ([x (for*/list ([a '(-1 0 1)]
                           [b '(-1 0 1)])
                 (cons (+ x a) (+ y b)))])
    (hash-ref hash x 0)))

(define (it-index->diff n)
  (case n
    [(0) (cons 1 0)]
    [(1) (cons 0 1)]
    [(2) (cons -1 0)]
    [(3) (cons 0 -1)]))


(define (solution-2 n)
  (let inner-loop ([vec-hash (hash '(0 . 0) 1)]
                   [x 0] [y 0])
    (let loop-2 ([vec vec-hash]
                 [x x] [y y]
                 [count 0] ;; repeat twice
                 [direction 0]
                 [iter-to 0])
      (let rep-loop ([vec vec] ;; repats n times twice, r u ll dd rrr uuu llll ... etc
                     [x x] [y y]
                     [iter 0])
        (match-let* ([val (sumn vec x y)]
                     [vec (hash-set vec (cons x y) val)]
                     [(cons dx dy) (it-index->diff direction)]
                     [x (+ dx x)] [y (+ dy y)])
          (cond
            [(val . > . n) val]
            [(iter . >= . iter-to)
             (if (count . = . 1)
                 (loop-2 vec x y 0 (modulo (add1 direction) 4) (add1 iter-to)) ;; iterated n times and looped twice, switch directions and also increment count
                 (loop-2 vec x y (add1 count) (modulo (add1 direction) 4) iter-to))] ;; iterated n times for this, switch directions
            [else (rep-loop vec x y (add1 iter))]))))))
