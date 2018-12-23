#lang racket

(provide f)

(define x 0.25)
(define (f p)
  (if (= p 0) (begin (set! x (- x 0.5)) (inexact->exact x))
      (begin (set! x (+ x 0.5)) (inexact->exact x))))