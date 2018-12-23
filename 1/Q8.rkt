#lang racket

(provide inverse)

;Define function inverse below
;Do not call the function here

(define (coeffs a b)
  (cond ((= b 0) (cons 1 0))
        ((> b a) (cons (cdr (coeffs b a)) (car (coeffs b a))))
      (else (let*([m (car (coeffs b (remainder a b)))]
            [n (cdr (coeffs b (remainder a b)))]
            [x n]
            [y (- m (* n (quotient a b)))])
        (cons x y)))))
(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))
(define (inverse e n)
  (if (= (gcd e n) 1) (let*([x (car (coeffs e n))])
                        (if(< x 0) (+ x n) x)) -1))
