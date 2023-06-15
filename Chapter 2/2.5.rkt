#lang sicp

(define (divides? a x)
    (= 0 (remainder x a)))


(define (cons a b)
    (* (expt 2 a) (expt 3 b)))


(define (num-of-factors x prod)
    (define (iter counter prod)
        (if (not (divides? x prod))
            counter
            (iter (+ counter 1) (/ prod x))))
    (iter 0 prod))

(define (car x)
    (num-of-factors 2 x))

(define (cdr x)
    (num-of-factors 3 x))

(car (cons 2321231 13214))
