#lang racket

(provide minchange)

;Define function minchange below
;Do not call the function here


(define (minchange n)
  (change-loop n 0))

(define (change-loop n count)
  (cond ((< n 0) 10000)
        ((= n 0) count)
        ((>= n 50) (change-loop (remainder n 50) (+ count (quotient n 50))))
        ((>= n 25) (let*([b (change-loop (- n 25) (+ count 1))]
                    [c (change-loop (- n 20) (+ count 1))])
                     (min b c)))
        ((>= n 20) (change-loop (- n 20) (+ count 1)))
        ((>= n 10) (change-loop (- n 10) (+ count 1)))
        ((>= n 5) (change-loop (- n 5) (+ count 1)))
        ((>= n 3) (change-loop (- n 3) (+ count 1)))
        ((>= n 2) (change-loop (- n 2) (+ count 1)))
        ((>= n 1) (change-loop (- n 1) (+ count 1)))))