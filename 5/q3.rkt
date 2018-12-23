#lang racket

(provide to-decimal)

(define (to-decimal l)
  (define state 0)
  (define negten 10)
  (define (helper l dpt)
    (cond ((null? l) state)
          ((char=? #\. (car l)) (helper (cdr l) #t))
          ((not dpt) (begin
                        (define x (- (char->integer (car l)) 48))
                        (set! state (* state 10))
                        (set! state (+ state x))
                        (helper (cdr l) #f)))
          (dpt (begin
                        (define x (- (char->integer (car l)) 48))
                        (set! state (+ state (/ x negten)))
                        (set! negten (* negten 10))
                        (helper (cdr l) #t)))))
  (exact->inexact (helper l #f)))