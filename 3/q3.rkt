#lang racket
(provide reverse-tr)
(define (myreverse l x)
  (if (= (length l) 0) x (myreverse (cdr l) (cons (car l) x))))

(define (reverse-tr l)
  (myreverse l '()))