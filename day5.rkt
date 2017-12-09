#lang racket

(provide (all-defined-out))

(struct tape
  (before
   current
   after)
  #:transparent)

(define (tape-flip t)
  (struct-copy tape t
               [before (tape-after t)]
               [after (tape-before t)]))

(define (tape-rotate n t)
  (cond [(zero? n) t]
        [(negative? n)
         (tape-flip (tape-rotate (abs n)
                                 (tape-flip t)))]
        [else
         (tape-rotate (sub1 n)
                      (tape (cons (tape-current t) (tape-before t))
                            (car (tape-after t))
                            (cdr (tape-after t))))]))

(define (tape-addn n t)
  (struct-copy tape t
               [current (+ n (tape-current t))]))

(define (should-exit t)
  (cond
    [(positive? (tape-current t))
     ((length (tape-after t)) . < . (tape-current t))]
    [(negative? (tape-current t))
     ((length (tape-before t)) . < . (abs (tape-current t)))]
    [else #f]))

(define (solve-1 lst)
  (let loop ([t (tape null (car lst) (cdr lst))]
             [n 1])
    (cond
      [(should-exit t) n]
      [else (loop (tape-rotate (tape-current t)
                               (tape-addn 1 t))
                  (add1 n))])))

(define (solution-1 str)
  (solve-1 (map string->number (string-split str))))

(define (solve-2 lst)
  (let loop ([t (tape null (car lst) (cdr lst))]
             [n 1])
    (cond
      [(should-exit t) n]
      [else (loop (tape-rotate (tape-current t)
                               (tape-addn (if ((tape-current t) . >= . 3) -1 1) t))
                  (add1 n))])))

(define (solution-2 str)
  (solve-2 (map string->number (string-split str))))
