#lang racket

(provide (all-defined-out))

(define line-regex #px"(\\w+)\\s\\((\\d+)\\)(?:\\s->\\s(\\w+(?:,\\s\\w+)*))?")

(struct node
  (name
   weight
   children
   parent)
  #:transparent)

(define (match-line line)
  (let ([matched (regexp-match line-regex line)])
    (list (second matched) (string->number (third matched))
          (if (fourth matched) (string-split (fourth matched) ", ")
              null))))

(define (build-node-for visited-nodes to-visit name creator to-remove)
  (if (hash-has-key? visited-nodes name)
      (values (hash-remove visited-nodes name) to-visit
              (struct-copy node (hash-ref visited-nodes name)
                           [parent creator])
              (cons name to-remove))
      ;; Otherwise we have to build it
      (let ([data (hash-ref to-visit name)]
            [to-visit (hash-remove to-visit name)])
        (let loop ([visited-nodes visited-nodes]
                   [to-visit to-visit]
                   [children (third data)]
                   [built-childs null]
                   [to-remove (cons name to-remove)])
          (if (null? children)
              (values visited-nodes to-visit
                      (node (first data)
                            (second data)
                            built-childs
                            creator)
                      to-remove)
              (let-values ([(visited-nodes to-visit child to-remove)
                            (build-node-for visited-nodes to-visit (car children) (first data)
                                            (cons (car children) to-remove))])
                (loop visited-nodes to-visit (cdr children) (cons child built-childs) to-remove)))))))

(define (build-tree lines)
  (let loop ([visited-nodes (hash)]
             [to-visit (foldl (lambda (x h) (hash-set h (car x) x)) (hash)
                              lines)]
             [lines (map car lines)])
    (if (null? lines)
        (car (hash-values visited-nodes)) ;; our table should be left with only one
        (let-values ([(visited-nodes to-visit built visited) (build-node-for visited-nodes to-visit (car lines) null null)])
          (loop (hash-set visited-nodes (car lines) built) to-visit (remove* visited lines))))))

(define (parse-lines str)
  (map match-line (string-split str "\n")))

(define (total-weight node)
  (+ (node-weight node)
     (apply + (map total-weight (node-children node)))))

(define (find-difference node)
  (let ([weights (map total-weight (node-children node))])
    (- (apply max weights) (apply min weights))))

(define (find-wrong-node node diff)
  (let loop ([children (node-children node)]
             [rest null])
    (if (null? children)
        node
        (let* ([current (car children)]
               [children (cdr children)]
               [weights (map total-weight (append children rest))]
               [current-weight (total-weight current)])
          (cond
            [(all-same (cons (current-weight . + . diff) weights)) ;; adding the diff made all these the same, must be correct
             (find-wrong-node current diff)]
            [(all-same (cons (current-weight . - . diff) weights))
             (find-wrong-node current (- diff))]
            [else
             (loop children (cons current rest))])))))

(define (all-same lst)
  (= (length (remove-duplicates lst)) 1))

(define (solution-1 str)
  (node-name
   (build-tree (parse-lines str))))

(define (solution-2 str)
  (let* ([tree (build-tree (parse-lines str))]
         [diff (find-difference tree)]
         [wrong-node (find-wrong-node tree diff)])
    (- (node-weight wrong-node) diff)))
