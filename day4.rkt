#lang racket

(define (solve-1 str)
  (not (check-duplicates (string-split str))))

(define (solution-1 str)
  (count solve-1 (string-split str "\n")))

(define string-reverse (compose list->string reverse string->list))

(define (make-anagrams str)
  (remove-duplicates (map list->string (permutations (string->list str)))))

(define (duplicate-anagrams lst)
  (flatten (map make-anagrams lst)))

(define solve-2
  (compose not check-duplicates duplicate-anagrams string-split))

(define (solution-2 str)
  (count solve-2 (string-split str "\n")))
