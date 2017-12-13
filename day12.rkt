#lang racket

(require data/queue)

(provide (all-defined-out))

(define reference-regex #px"(\\d+) <-> (\\d+(?:,\\s\\d+)*)")

(define (parse-line str)
  (match-let ([(list _ n c) (regexp-match reference-regex str)])
    (cons n (string-split c ", "))))

(define (bfs mapping start [exclude null])
  (let ([queue (make-queue)])
    (for ([i (hash-ref mapping start)])
      (enqueue! queue i))
    (let loop ([visited (list start)])
      (if (queue-empty? queue)
          (cons #t visited)
          (let ([vis (dequeue! queue)])
            (cond [(member vis exclude) (cons #f null)]
                  [(member vis visited) (loop visited)]
                  [else
                   (for ([i (hash-ref mapping vis)])
                     (enqueue! queue i))
                   (loop (cons vis visited))]))))))

(define (solution-1 str start)
  (length (cdr
           (bfs (make-immutable-hash
                 (map parse-line (string-split str "\n")))
                start))))

(define (solution-2 str)
  (let ([graph (make-immutable-hash
                (map parse-line (string-split str "\n")))])
    (let loop ([remaining (hash-keys graph)]
               [visited null]
               [count 0])
      (if (null? remaining)
          count
          (match-let ([(cons sucs visit) (bfs graph (car remaining) visited)])
            (if sucs
                (loop (remove* visit remaining)
                      (append visit visited)
                      (add1 count))
                (loop (cdr remaining)
                      visited
                      count)))))))
