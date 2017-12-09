#lang racket

(provide (all-defined-out))

(define (distribute index lst)
  (let ([len (length lst)])
    (let loop ([state (list-set lst index 0)]
               [n (list-ref lst index)]
               [index (modulo (add1 index) len)])
      (if (zero? n)
          state
          (loop (list-update state index add1)
                (sub1 n)
                (modulo (add1 index) len))))))

(define (solve-1-2 lst)
  (let loop ([combinations (hash)]
             [n 0]
             [state lst])
    (let ([index (index-of state (apply max state))])
      (if (hash-has-key? combinations state)
          (values n (n . - . (hash-ref combinations state)))
          (let ([newstate (distribute index state)])
            (loop (hash-set combinations state n)
                  (add1 n)
                  newstate))))))

(define (solution-1-2 str)
  (solve-1-2 (map string->number (string-split str))))
