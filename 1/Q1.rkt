#lang racket

(provide has-solution)

;Define function has-solution below
;Do not call the function here
(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))
(define (has-solution a b c)
  (if (= (remainder c (gcd a b)) 0) #t
      #f))
