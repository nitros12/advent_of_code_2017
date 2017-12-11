#lang racket

(provide (all-defined-out))

(define (list-flip lst i n)
  (let ([lst (list-rotate lst i)])
    (let ([lst (append (reverse (take lst n)) (drop lst n))])
      (list-rotate-right lst i))))

(define (list-rotate lst i)
  (let-values ([(start end) (split-at lst i)])
    (append end start)))

(define (list-rotate-right lst i)
  (let-values ([(start end) (split-at-right lst i)])
    (append end start)))

(define (solve-1 inp size)
  (let loop ([inp inp]
             [lst (range size)]
             [skip 0]
             [pos 0])
    (if (null? inp)
        lst
        (let ([lst (list-flip lst pos (car inp))])
          (loop
           (cdr inp)
           lst
           (add1 skip)
           (modulo (+ pos
                      (car inp)
                      skip) size))))))

(define (solution-1 inp)
  (apply * (take (solve-1
                  (map string->number
                       (string-split inp ","))
                  256)
                 2)))
