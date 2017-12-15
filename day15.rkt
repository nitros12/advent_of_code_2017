#lang racket

(require racket/generator)
(require racket/fixnum)

(provide (all-defined-out))

(define (mkgen-1 start factor)
  (generator ()
             (let loop ([val start])
               (let ([next (fxremainder (fx* val factor)
                                        2147483647)])
                 (yield next)
                 (loop next)))))

(define (solve-gen len gena genb)
  (for/fold ([c 0])
            ([i (in-range len)])
    (when (fx= (fxremainder i 100000) 0)
      (println i))
    (if (fx= (bitwise-bit-field (gena) 0 16)
             (bitwise-bit-field (genb) 0 16))
        (add1 c)
        c)))

(define (mkgen-2 start factor divisor)
  (generator ()
             (let ([gen (mkgen-1 start factor)])
               (let loop ([x (gen)])
                 (when (fx= (fxremainder x divisor) 0)
                   (yield x))
                 (loop (gen))))))

(define (solve-1 a b)
  (let ([gena (mkgen-1 a 16807)]
        [genb (mkgen-1 b 48271)])
    (solve-gen 40000000 gena genb)))

(define (solve-2 a b)
  (let ([gena (mkgen-2 a 16807 4)]
        [genb (mkgen-2 b 48271 8)])
    (solve-gen 5000000 gena genb)))
