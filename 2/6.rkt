#lang racket

(provide estimate-integral)

;Define function estimate-integral below
;Do not call the function here

(define (scaled-random x1 x2)
(+ x1 (* (random) (- x2 x1))))

(define (est-loop P x1 x2 y1 y2 k n sat)
  (cond ((= k n) (* (/ sat n) (abs (- x2 x1)) (abs (- y2 y1))))
        ((P (scaled-random x1 x2) (scaled-random y1 y2)) (est-loop P x1 x2 y1 y2 (+ k 1) n (+ sat 1)))
        (else (est-loop P x1 x2 y1 y2 (+ k 1) n sat))))

(define (estimate-integral P x1 x2 y1 y2 n)
  (est-loop P x1 x2 y1 y2 0 n 0))

(define (circle-checker x y) (<= (+ (* x x) (* y y)) 1))
