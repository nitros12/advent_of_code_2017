#lang racket

(provide (all-defined-out))

(define (does-hit pos len)
  (= (modulo pos
             (- (* 2
                   len)
                2))
     0))

(define (get-severity pos len)
  ;; Reaches 0 every 2n-2 cycles
  (if (does-hit pos len)
      (* pos len)
      0))

(define (get-values str)
  (map (lambda (x) (map string->number (string-split x ": ")))
       (string-split str "\n")))

(define (solution-1 str)
  (apply + (map (lambda (x)
                  (get-severity (first x) (second x)))
                (get-values str))))

(define (solution-2 str) ;; exhaustive solution is faster than finding what times are valid
  (let* ([vals (get-values str)])
    (let loop ([c 0])
      (cond
        [(ormap (lambda (x)
                  (does-hit (+ c (first x))
                            (second x)))
                vals)
         (loop (add1 c))]
        [else c]))))
