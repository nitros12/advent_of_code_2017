#lang racket

(require "utils.rkt")

(define (solve-1 rows)
  (apply + (map (lambda (x)
                  ((apply max x) . - . (apply min x)))
                rows)))

(define (solution-1 str)
  (solve-1 (strarray->numarray str)))

(define (ifdivisible a b)
  (if (integer? (a . / . b))
      (a . / . b) 0))

(define (solve-2 rows)
  (apply + (map (lambda (x)
                  (for/sum ([y (combinations x 2)])
                    (let ([a (first y)]
                          [b (second y)])
                      (+ (ifdivisible a b)
                         (ifdivisible b a)))))
                rows)))

(define (solution-2 str)
  (solve-2 (strarray->numarray str)))
