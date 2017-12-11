#lang racket

(provide (all-defined-out))

(define instr-regex #px"(\\w+) (inc|dec) (-?\\d+) if (\\w+) ([<>!=]=?) (-?\\d+)")

(struct instr (reg op num cmpreg cmpop comp) #:transparent)

(define (!= . a) (not (apply = a)))

(define == =) ;; wew

(define (parse-line str)
  (match-let ([(list _ reg op incr cmpreg cmpop comp) (regexp-match instr-regex str)])
    (instr reg (if (string=? op "inc") '+ '-) (string->number incr) cmpreg (string->symbol cmpop) (string->number comp))))

(define (eval-instruction regs instr)
  (let ([cmpnum (hash-ref regs (instr-cmpreg instr) 0)])
    (if (eval (list (instr-cmpop instr) cmpnum (instr-comp instr)))
        (hash-set regs (instr-reg instr)
                  (eval (list (instr-op instr) (hash-ref regs (instr-reg instr) 0) (instr-num instr))))
        regs)))

(define (evaluate lst)
  (let loop ([regs (hash)]
             [instrs lst]
             [max-val 0])
    (if (null? instrs)
        (values (apply max (hash-values regs)) max-val)
        (loop (eval-instruction regs (car instrs)) (cdr instrs) (apply max (cons max-val (hash-values regs)))))))

(define (solution-1-2 str)
  (evaluate
   (map parse-line (string-split str "\n"))))
