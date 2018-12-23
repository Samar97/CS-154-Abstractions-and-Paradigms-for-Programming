#lang racket

(provide modexp)

;Define function modexp below
;Do not call the function here

(define (modexp x y n)
  (cond ((= y 1) (remainder x n))
    ((= (remainder y 2) 0) (remainder (* (modexp x (/ y 2) n) (modexp x (/ y 2) n)) n))
    (else (remainder (* (remainder x n) (modexp x (- y 1) n)) n))))

