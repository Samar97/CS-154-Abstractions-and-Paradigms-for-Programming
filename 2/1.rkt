#lang racket

(provide decode)

;Define function decode below
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

(define (modexp x y n)
  (cond ((= y 1) (remainder x n))
  	((= y 2) (remainder (* x x) n))
    ((= (remainder y 2) 0) (remainder (modexp (modexp x (/ y 2) n) 2 n) n))
    (else (remainder (* (remainder x n) (modexp x (- y 1) n)) n))))

(define (decode p q e c)
  (let*([phi (* (- p 1) (- q 1))]
        [d (inverse e phi)]
        [N (* p q)])
    (modexp c d N)))
