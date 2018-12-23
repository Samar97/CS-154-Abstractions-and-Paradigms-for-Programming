#lang racket
(provide summands)
(define (summands n)
  (cond [(= n 1) '((1))]
        [else (append (map (lambda(x) (cons 1 x)) (summands (- n 1))) (map (lambda(x) (cons (+ (car x) 1) (cdr x))) (summands (- n 1))))]))