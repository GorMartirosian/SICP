#lang sicp

(define (fast-expt b n)
    (expt-iter b n 1))
(define (square x) (* x x))
(define (even? x) ( = (remainder x 2) 0))

(define (expt-iter b n a)
    (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))))

(fast-expt 14 4)