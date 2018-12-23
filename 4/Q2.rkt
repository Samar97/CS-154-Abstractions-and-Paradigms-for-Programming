#lang racket

(struct node (value ltree rtree) #:transparent)
(struct nulltree () #:transparent)

(define t1
  (node 10
        (node 10
              (node 5 (nulltree) (nulltree))
              (nulltree))
        (node 15 (nulltree) (nulltree))))

(define (list-within t lb ub)
  (cond ((and (node? t) (and (<= lb (node-value t)) (>= ub (node-value t)))) (append (list-within (node-ltree t) lb ub) (list (node-value t))
                        (list-within (node-rtree t) lb ub)))
        ((and (node? t) (> lb (node-value t)))
         (list-within (node-rtree t) lb ub))
        ((and (node? t) (< ub (node-value t)))
         (list-within (node-ltree t) lb ub))
      (else '())))
