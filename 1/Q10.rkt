#lang racket

(provide goldbach)

;Define function goldbach below
;Do not call the function here

(define (modexp x y n)
  (cond ((= y 1) (remainder x n))
    ((= (remainder y 2) 0) (remainder (* (modexp x (/ y 2) n) (modexp x (/ y 2) n)) n))
    (else (remainder (* (remainder x n) (modexp x (- y 1) n)) n))))
(define (is-prime n)
  (if (= n 1) #f (modchecker n 200)))
(define (modchecker n i)
  (cond ((= i 0) #t)
      (else (let*([a (+ (random (- n 1)) 1)])
              (if (= (modexp a (- n 1) n) 1) (modchecker n (- i 1))
                  #f)))))
(define (gold m n)
  (if (and (is-prime m) (is-prime n)) (cons m n)
      (gold (- m 2) (+ n 2))))
(define (goldbach m)
  (gold (- m 3) 3))
