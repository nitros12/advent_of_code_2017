#lang racket

(provide (all-defined-out))

(struct state
  (text
   depth
   score
   garbage
   eating-garbage)
  #:transparent)

(define (run lst)
  (letrec ([enter-garbage
            (lambda (st)
              (eat-character (struct-copy state st
                                          [eating-garbage #t])))]
           [exit-garbage
            (lambda (st)
              (eat-character (struct-copy state st
                                          [eating-garbage #f])))]
           [escape
            (lambda (st)
              (eat-character (struct-copy state st
                                          [text (if (null? (state-text st))
                                                    null
                                                    (cdr (state-text st)))])))]
           [enter-group
            (lambda (st)
              (eat-character (struct-copy state st
                                          [depth (add1 (state-depth st))]
                                          [score (+ (state-depth st) (state-score st))])))]
           [exit-group
            (lambda (st)
              (eat-character (struct-copy state st
                                          [depth (sub1 (state-depth st))])))]
           [eat-garbage (lambda (st)
                          (eat-character (struct-copy state st
                                                      [garbage (add1 (state-garbage st))])))]
           [eat-character (lambda (st)
                            (if (null? (state-text st))
                                st
                                ((cond
                                   [(state-eating-garbage st)
                                    (case (car (state-text st))
                                      [(#\!) escape]
                                      [(#\>) exit-garbage]
                                      [else eat-garbage])]
                                   [else
                                    (case (car (state-text st))
                                      [(#\!) escape]
                                      [(#\<) enter-garbage]
                                      [(#\{) enter-group]
                                      [(#\}) exit-group]
                                      [else eat-character])])
                                 (struct-copy state st
                                              [text (cdr (state-text st))]))))])
    (eat-character (state lst 1 0 0 #f))))

(define (solution-1-2 str)
  (run (string->list str)))
