#lang racket

(provide while)

(define-syntax while
  (syntax-rules ()
      [(while boolexp statements state ...)
       (begin
         (define (loop)
           (cond (boolexp (begin statements state ... (loop)))))
         (cond (boolexp (loop))))]
    [(while boolexp) (void)]))