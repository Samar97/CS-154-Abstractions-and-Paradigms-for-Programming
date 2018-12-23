#lang racket

(provide ak-mult)

;Define function ak-mult below
;Do not call the function here

(define (mul x y c)
  (cond
    ((= x 1) (+ y c))
    ((= (remainder x 2) 0) (mul (/ x 2) (* y 2) c))
    (else (mul (- x 1) y (+ y c)))))
(define (ak-mult x y)
  (mul x y 0))

