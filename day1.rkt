#lang racket

(define (rotate-left l n)
  (let-values ([(a b) (split-at l n)])
    (append b a)))

(define (extend l)
  (cons (car l) (reverse l)))

(define (string->listofnum str)
  (map (lambda (x) ((char->integer x) . - . (char->integer #\0))) (string->list str)))

(define (calculate-1 inp)
  (let loop ([sum 0]
             [l (extend inp)])
    (if ((length l) . > . 1)
        (match (take l 2)
          [(list x x) (loop (+ sum x) (cdr l))]
          [_ (loop sum (cdr l))])
        sum)))

(define (calculate-2 inp)
  (let ([rot (rotate-left inp ((length inp) . / . 2))])
    (let loop ([sum 0]
               [a (extend inp)]
               [b (extend rot)])
      (if ((length a) . > . 1)
          (match (cons (car a) (car b))
            [(cons x x) (loop (+ sum x) (cdr a) (cdr b))]
            [_ (loop sum (cdr a) (cdr b))])
          sum))))

(define solution-1 (compose calculate-1 string->listofnum))

(define solution-2 (compose calculate-2 string->listofnum))
