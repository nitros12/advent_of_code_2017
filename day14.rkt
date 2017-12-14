#lang racket

(require "day10.rkt"
         racket/format)

(provide (all-defined-out))

(define (padded-binary lst)
  (string->list
   (apply string-append
          (map
           (lambda (n) (~r n #:base 2 #:min-width 8 #:pad-string "0"))
           lst))))

(define (general str)
  (let* ([keys (map (lambda (x) (format "~a-~a" str x))
                    (range 128))]
         [hashes (map knot-hash keys)])
    (map padded-binary hashes)))

(define (solution-1 str)
  (apply +
         (map (lambda (x)
                (count (lambda (n)
                         (char=? n #\1))
                       x))
              (general str))))

(define (list->matrix rows)
  (list->vector (map list->vector rows)))

(define (matrix-ref mat c)
  (vector-ref (vector-ref mat (car c)) (cdr c)))

(define offsets
  '((1 . 0)
    (-1 . 0)
    (0 . 1)
    (0 . -1)))

(define ((vec-add a) b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

(define (in-range? x start stop)
  (and (stop  . > . x)
       (start . <= . x)))

(define (gen-offsets a)
  (filter (lambda (x) (and (in-range? (car x) 0 128)
                           (in-range? (cdr x) 0 128)))
          (map (vec-add a)
               offsets)))

(define (flood-fill grid start)
  (let loop ([stack (list start)]
             [visited (set)]
             [in-group (set)])
    (if (null? stack)
        (values visited in-group)
        (cond
          [(char=? (matrix-ref grid (car stack)) #\0)
           (loop (cdr stack)
                 (set-add visited (car stack))
                 in-group)]
          [(set-member? visited (car stack))
           (loop (cdr stack)
                 (set-add visited (car stack))
                 in-group)]
          [else
           (loop (append (gen-offsets (car stack))
                         (cdr stack))
                 (set-add visited (car stack))
                 (set-add in-group (car stack)))]))))

(define (generate-coords x y)
  (for*/list ([i (range x)]
              [j (range y)])
    (cons i j)))

(define (build-groups rows)
  (let ([grid (list->matrix rows)])
    (let loop ([groups (hash)]
               [visited (set)]
               [coords (generate-coords 128 128)]
               [i 1])
      (if (null? coords)
          groups
          (let-values ([(new-visited new-group)
                        (flood-fill grid (car coords))])
            (loop
             (if (set-empty? new-group)
                 groups
                 (hash-set groups i new-group))
             (set-union visited new-visited)
             (remove* (set->list new-visited) coords)
             (add1 i)))))))

(define solution-2
  (compose length hash-keys build-groups general))
