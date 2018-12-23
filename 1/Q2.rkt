#lang racket

(provide sub sub-single)

;Define functions sub and sub-single below
;Do not call the function here

(define (convert m n)
  (+ (* m 10) n))
(define (sub-single x y z)
  (if (and (< x 10) (< y 10) (or(and (= z 0) (> x (- y 1))) (and (= z 1) (< x y))) (> y -1) (> x -1) (> z -1) (< z 2)) (convert z (- x y)) ""))
(define (sub x y)
  (if (= y 0) x (let*([qx (quotient x 10)]
        [qy (quotient y 10)]
        [rx (remainder x 10)]
        [ry (remainder y 10)]
        [cy (if (< rx ry) 1 0)])
    (convert (sub (- qx cy) qy) (sub-single rx ry cy)))))
