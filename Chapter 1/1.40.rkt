#lang sicp


(define (cubic a b c)
    (define (square x) (* x x))
    (define (cube x) (* x x x))
    (lambda (x) 
        (+ (cube x)  (* a (square x)) (* b x) c)))

(define (k) (cubic 1 1 1))
(define f (cubic 1 1 1))
((k) 1)
