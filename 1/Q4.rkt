#lang racket

(provide div)

;Define function div below
;Do not call the function here

(define (div x y)
  (cond ((< x y) (cons 0 x))
      ((= (remainder x 2) 0) (let*([a2 (if (> (* (cdr (div (/ x 2) y)) 2) (- y 1)) (- (* (cdr (div (/ x 2) y)) 2) y)
                                          (* (cdr (div (/ x 2) y)) 2))]
                                  [a1 (if (> (* (cdr (div (/ x 2) y)) 2) (- y 1)) (+ (* (car (div (/ x 2) y)) 2) 1)
                                          (*(car (div (/ x 2) y)) 2))])
                              (cons a1 a2))) 
      (else (let*([sumcdr (+ (cdr(div (/ (- x 1) 2) y)) (cdr(div (/ (+ x 1) 2) y)) )]
                  [sumcar (+ (car(div (/ (- x 1) 2) y)) (car(div (/ (+ x 1) 2) y)) )]
                  [a2 (if (> sumcdr (- y 1)) (- sumcdr y)
                          sumcdr )]
                  [a1 (if (> sumcdr (- y 1)) (+ sumcar 1)
                          sumcar )])
              (cons a1 a2)))))

