#lang racket

(provide make-account make-joint)

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
    (cond [(and (eq? m `joint) (not (eq? p pass))) #f] 
          [(not (eq? p pass)) wrongpass]
          [(eq? m `withdraw) withdraw]
          [(eq? m `deposit) deposit]
          [(eq? m `joint) #t]
          [else (error "Unknown request -- " m)]))
  dispatch)

(define (make-joint account oldpass newpass)
  (begin
  (define status (account oldpass `joint))
  (cond ((not status) (error "Incorrect password")))
  (define (wrongpass amount)
    (error "Incorrect password"))
  (define (dispatch p m)
    (cond [(not (eq? p newpass)) wrongpass]
          [(eq? m `withdraw) (account oldpass `withdraw)]
          [(eq? m `deposit) (account oldpass `deposit)]
          [(eq? m `joint) (account oldpass `joint)]
          [else (error "Unknown request -- " m)]))
  (cond (status dispatch)
        (else (error "Incorrect password -- Can't create joint account")))))