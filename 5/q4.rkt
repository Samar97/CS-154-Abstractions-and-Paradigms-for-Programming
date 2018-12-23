#lang racket

(provide make-account)

(define (make-account balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (wrongpass amount)
    (error "Incorrect password"))
  (define (dispatch p m)
    (cond [(not (eq? p pass)) wrongpass]
          [(eq? m `withdraw) withdraw]
          [(eq? m `deposit) deposit]
          [else (error "Unknown request -- " m)]))
  dispatch)
