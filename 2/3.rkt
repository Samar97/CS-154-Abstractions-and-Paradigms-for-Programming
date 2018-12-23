#lang racket

(provide simpson)

;Define function simpson below
;Do not call the function here

(define (series f a b n h k res)
  (cond ((= k 0) (series f a b n h (+ k 1) (+ res (f (+ a (* k h))))))
        ((= k (+ n 1)) res)
        ((= k n) (series f a b n h (+ k 1) (+ res (f (+ a (* k h))))))
        ((= (remainder k 2) 1) (series f a b n h (+ k 1) (+ res (* (f (+ a (* k h))) 4))))
        (else (series f a b n h (+ k 1) (+ res (* (f (+ a (* k h))) 2))))))

(define (simpson f a b n)
  (let*([h (/ (- b a) n)])
    (* (/ h 3) (series f a b n h 0 0))))



