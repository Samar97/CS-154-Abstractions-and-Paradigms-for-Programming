#lang racket
(provide gc)
(define (gc n)
  (if (= n 1) '((0) (1))
      (append (map (lambda(x) (cons 0 x)) (gc (- n 1))) (map (lambda(x) (cons 1 x)) (reverse (gc (- n 1)))))))