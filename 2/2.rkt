#lang racket

(provide filtered-accumulate)
(provide f0)
(provide f1)
(provide f2)
(provide f3)
(provide f4)

;Define all functions below
;Do not call the functions here

(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (is-coprime a b)
  (= (gcd a b) 1))

(define (modexp x y n)
  (cond ((= y 1) (remainder x n))
  	((= y 2) (remainder (* x x) n))
    ((= (remainder y 2) 0) (remainder (modexp (modexp x (/ y 2) n) 2 n) n))
    (else (remainder (* (remainder x n) (modexp x (- y 1) n)) n))))

(define (is-prime n)
  (if (= n 1) #f (modchecker n 200)))

(define (modchecker n i)
  (cond ((= i 0) #t)
      (else (let*([a (+ (random (- n 1)) 1)])
              (if (= (modexp a (- n 1) n) 1) (modchecker n (- i 1))
                  #f)))))

(define (fac-loop x n res)
  (if (> x n) res (fac-loop (+ x 1) n (* res x))))

(define (factorial n)
  (fac-loop 1 n 1))

(define (identity a) a)

(define (square a) (* a a))

(define (filtered-accumulate prop op form a b res)
  (cond ((> a b) res)
        ((prop a) (filtered-accumulate prop op form (+ a 1) b (op (form a) res)))
        (else (filtered-accumulate prop op form (+ a 1) b res))))

(define (f0 a b)
  (filtered-accumulate (lambda(p) (= (remainder p 13) (remainder a 13))) + identity a b 0))

(define (f1 a b)
  (filtered-accumulate (lambda(p) (= (remainder p 2) 1)) + square a b 0))

(define (f2 a b)
  (filtered-accumulate (lambda(p) (= (remainder p 3) 0)) * factorial a b 1))

(define (f3 a b)
  (filtered-accumulate is-prime + square a b 0))

(define (f4 a)
  (filtered-accumulate (lambda(p) (is-coprime a p)) * identity 1 a 1))


