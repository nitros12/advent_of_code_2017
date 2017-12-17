#lang racket

(provide (all-defined-out))

(define (circular-ref lst len i)
  (list-ref lst (modulo i len)))

(define (insert-at lst len i val)
  (let ([i (modulo i len)])
    (let-values ([(left right) (split-at lst i)])
      (append left (cons val right)))))

(define (general steps iter-to)
  (let loop ([i 1]
             [p 0]
             [lst '(0)])
    (if (= iter-to i)
        lst
        (loop (add1 i)
              (modulo (+ 1 p steps) (add1 i))
              (insert-at lst i p i)))))

(define (solution-1 steps)
  (let ([lst (general steps 2018)])
    (circular-ref lst 2018 (add1 (index-of lst 2017)))))

(define (solution-2 steps)
  (let loop ([i 1]
             [p 0]
             [s 1])
    (cond
      [(= 50000001 i) s]
      [(= 0 p) (loop (add1 i)
                     (modulo (+ 1 steps p) (add1 i))
                     i)]
      [else (loop (add1 i)
                  (modulo (+ 1 steps p) (add1 i))
                  s)])))
