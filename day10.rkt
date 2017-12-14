#lang racket

(require racket/format)

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

(define (solve-1-2 inp size)
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
  (apply * (take (solve-1-2
                  (map string->number
                       (string-split inp ","))
                  256)
                 2)))

(define (duplicate lst n)
  (let loop ([res null]
             [n n])
    (if (n . > . 0)
        (loop (append lst res) (sub1 n))
        res)))

(define (chunk lst n)
  (let loop ([res null]
             [lst lst])
    (if (null? lst)
        (reverse res)
        (let-values ([(a b) (split-at lst n)])
          (loop (cons a res) b)))))

(define (sparse->dense lst)
  (for/list ([i (chunk lst 16)])
    (apply bitwise-xor i)))

(define (knot-hash str)
  (sparse->dense
   (solve-1-2
    (duplicate
     (append
      (map char->integer (string->list str))
      '(17 31 73 47 23))
     64)
    256)))

(define (solution-2 inp)
  (let* ([sparse-hash
         (solve-1-2 (duplicate
                     (append
                      (map char->integer (string->list inp))
                      '(17 31 73 47 23))
                     64)
                    256)]
         [dense-hash (sparse->dense sparse-hash)])
    (apply string-append (map (lambda (n) (~r n #:base 16 #:min-width 2 #:pad-string "0"))
                              dense-hash))))
