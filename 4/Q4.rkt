#lang racket
(struct gnode (val lst) #:transparent)

(define t1 (gnode 5 '()))
(define t2 (gnode 4 '()))
(define t3 (gnode 3 '()))
(define t4 (gnode 2 '()))
(define t5 (gnode 1 '()))

(define s1 (gnode 15 '()))
(define s2 (gnode 14 '()))
(define s3 (gnode 13 '()))
(define s4 (gnode 12 '()))
(define s5 (gnode 11 '()))

(define m1 (gnode 10 (list t1 t2 t3 t4)))
(define m2 (gnode 20 (list t5 s5 s4)))
(define m3 (gnode 30 (list s1 s2 s3)))

(define gtree (gnode 50 (list m1 m2 s1 s2)))

(define (lev-helper t n)
  (cond ((and (not (gnode? t)) (> n 0)) '())
        ((and (gnode? t) (= n 1)) (list (gnode-val t)))
        (else (flatten (map (lambda(x) (lev-helper x (- n 1))) (gnode-lst t))))))

(define (atlevel t n)
  (lev-helper t n))