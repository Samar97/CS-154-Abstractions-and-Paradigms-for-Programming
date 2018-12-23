#lang racket

(provide simplify)
(provide add)
(provide multiply)
(provide divide)

;Define all functions below
;Do not call the functions here

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (simplify r)
  (let*([n (car r)]
        [d (cdr r)]
        [div (gcd n d)]
        [p (/ n div)]
        [q (/ d div)])
    (cons p q)))

(define (add r1 r2)
  (let*([n1 (car r1)]
        [n2 (car r2)]
        [d1 (cdr r1)]
        [d2 (cdr r2)]
        [p (+ (* n1 d2) (* n2 d1))]
        [q (* d1 d2)])
        (simplify (cons p q))))

(define (multiply r1 r2)
  (let*([n1 (car r1)]
        [n2 (car r2)]
        [d1 (cdr r1)]
        [d2 (cdr r2)]
        [p (* n1 n2)]
        [q (* d1 d2)])
    (simplify (cons p q))))

(define (divide r1 r2)
  (let*([n1 (car r1)]
        [n2 (car r2)]
        [d1 (cdr r1)]
        [d2 (cdr r2)]
        [p (* n1 d2)]
        [q (* d1 n2)])
  (simplify (cons p q))))