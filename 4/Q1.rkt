#lang racket

(struct node (ltree val rtree) #:transparent)
(struct leaf (val) #:transparent)

(define t1 (leaf "a"))
(define t2 (leaf "b"))
(define t3 (leaf "c"))
(define s4 (leaf "d"))
(define m3 (node t3 "e" s4))
(define t4 (node t1 "d" t2))
(define t5 (node t4 "e" m3))
(define t11 (leaf "a"))
(define t12 (leaf "b"))
(define t13 (leaf "c"))
(define s14 (leaf "d"))
(define m13 (node t13 "e" s14))
(define t14 (node t11 "d" t12))
(define t15 (node t14 "e" m13))
(define tre (node t5 "s" t15))


(define (dia-helper t)
  (cond ((leaf? t) '(0 0 1))
        (else (let*([a (dia-helper (node-ltree t))]
                    [b (dia-helper (node-rtree t))]
                    [ml (max (car a) (cadr a))]
                    [mr (max (car b) (cadr b))])
                (list (+ ml 1) (+ mr 1) (max (caddr a) (caddr b) (+ 3 ml mr)))))))

(define (dia t)
  (caddr (dia-helper t)))