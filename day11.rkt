#lang racket

(provide (all-defined-out))

(define (run lst)
  (for/fold ([x 0]
             [y 0]
             [max-dis 0])
            ([i lst])
    (let-values ([(x y)
                  (case i
                    [("n")  (values       x  (sub1 y))]
                    [("ne") (values (add1 x) (sub1 y))]
                    [("se") (values (add1 x)        y)]
                    [("s")  (values       x  (add1 y))]
                    [("sw") (values (sub1 x) (add1 y))]
                    [("nw") (values (sub1 x)       y)])])
      (values x y (max (axial-displacement x y)
                       max-dis)))))

(define (axial-displacement x y)
  (/ (+ (abs x)
        (abs (+ x y))
        (abs y))
     2))

(define (solution-1 str)
  (let-values ([(x y _)
                (run (string-split str ","))])
    (axial-displacement x y)))

(define (solution-2 str)
  (let-values ([(x y disp)
                (run (string-split str ","))])
    disp))
