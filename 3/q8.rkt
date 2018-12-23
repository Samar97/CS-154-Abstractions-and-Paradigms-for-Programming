#lang racket
(provide lcs)
(define (lcs l1 l2)
  (cond ((or (null? l1) (null? l2)) '())
        ((= (car l1) (car l2)) (cons (car l1) (lcs (cdr l1) (cdr l2))))
        (else (let*([y (lcs l1 (cdr l2))]
                    [z (lcs l2 (cdr l1))])
                (if (> (length y) (length z)) y z)))))