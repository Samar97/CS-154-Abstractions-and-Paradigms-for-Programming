#lang racket
(provide rle)
(define (rle l)
  (if (null? l) '() (rle-helper (cdr l) (list (list (car l) 1)))))

(define (rle-helper l x)
  (if (null? l) (reverse x)
      (if (= (car l) (car (car x)))
          (let*([y (append (list (list (car (car x)) (+ (car (cdr (car x))) 1))) (cdr x))])
            (rle-helper (cdr l) y))
          (let*([z (append (list (list (car l) 1)) x)])
            (rle-helper (cdr l) z)))))