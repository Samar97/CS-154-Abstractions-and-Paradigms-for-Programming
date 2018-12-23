#lang racket

(provide carmichael)

;Define function carmichael below
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
(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (carloop m n)
  (cond ((= m n) #t)
        ((= (gcd m n) 1) (if (= (modexp m (- n 1) n) 1) (carloop (+ m 1) n)
                             #f))
        (else (carloop (+ m 1) n))))
(define (carcheck x y z)
      (cond ((= x y) (- z 1))
        ((is-prime z) (carcheck x y (+ z 1)))
          ((carloop 2 z) (carcheck (+ x 1) y (+ z 1)))
          (else (carcheck x y (+ z 1)))))

(define (carmichael n)
  (carcheck 0 n 4))
