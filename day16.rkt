#lang racket

(provide (all-defined-out))

(define (parse-instr str)
  (case (substring str 0 1)
    [("s") (spin (string->number (substring str 1)))]
    [("x") (apply exchange (map string->number (string-split (substring str 1) "/")))]
    [("p") (apply partner (map (compose car string->list)
                               (string-split (substring str 1) "/")))]))

(define ((spin i) lst)
  (let-values ([(start end) (split-at-right lst i)])
    (append end start)))

(define ((exchange a b) lst)
  (list-set (list-set lst a (list-ref lst b))
            b (list-ref lst a)))

(define ((partner a b) lst)
  ((exchange
    (index-of lst a)
    (index-of lst b))
   lst))

(define (build-instructions str)
  (map (compose parse-instr string-trim)
       (string-split str ",")))

(define (solution-1 prog str)
  (list->string
   (for/fold ([l (string->list prog)])
             ([in (build-instructions str)])
     (in l))))

(define (solution-2 prog str iter)
  (list->string
   (let* ([instrs (build-instructions str)]
          [prog (string->list prog)])
     (let loop ([c 1]
                [l prog]
                [prev null])
       (let ([nl
              (for/fold ([l l])
                        ([in instrs])
                (in l))])
         (cond
           [(and (equal? nl prog) (c . > . (remainder iter c)))
            (list-ref (reverse prev) (remainder iter c))]
           [(= c iter) nl]
           [else (loop (add1 c) nl (cons l prev))]))))))
