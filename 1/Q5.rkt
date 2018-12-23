#lang racket

(provide coeffs)

;Define function coeffs below
;Do not call the function here
(define (coeffs a b)
  (cond ((= b 0) (cons 1 0))
        ((> b a) (cons (cdr (coeffs b a)) (car (coeffs b a))))
      (else (let*([m (car (coeffs b (remainder a b)))]
            [n (cdr (coeffs b (remainder a b)))]
            [x n]
            [y (- m (* n (quotient a b)))])
        (cons x y)))))

