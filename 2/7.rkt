#lang racket

(provide fib-lightning)
(provide fib-tr)

;Define all functions below
;Do not call the functions here

(define (fib-loop a b n k)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        ((= k n) b)
        (else (fib-loop b (+ a b) n (+ k 1)))))

(define (fib-tr n)
  (fib-loop 1 1 n 1))

(define (mat-mult x y)
  (let*([a (car x)]
        [b (cadr x)]
        [c (caddr x)]
        [d (cadddr x)]
        [e (car y)]
        [f (cadr y)]
        [g (caddr y)]
        [h (cadddr y)]
        [p (+ (* a e) (* b g))]
        [q (+ (* a f) (* b h))]
        [r (+ (* c e) (* d g))]
        [s (+ (* c f) (* d h))])
    (list p q r s)))

(define (mat-exp x n)
  (cond ((= n 0) (list 1 0 0 1))
        ((= n 1) x)
        ((= n 2) (mat-mult x x))
        ((= (remainder n 2) 0) (mat-exp (mat-exp x (/ n 2)) 2))
        (else (mat-mult x (mat-exp x (- n 1))))))

(define (fib-lightning n)
  (car (mat-exp (list 1 1 1 0) n)))