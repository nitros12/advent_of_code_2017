#lang racket

(provide string->listofnum
         strarray->numarray)

(define (string->listofnum str)
  (map (lambda (x) ((char->integer x) . - . (char->integer #\0))) (string->list str)))

(define (strarray->numarray str)
  (map (lambda (r) (map string->number (string-split r)))
       (string-split str "\n")))
