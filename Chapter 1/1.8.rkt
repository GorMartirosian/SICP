#lang sicp

(define (cbrt-iter guess x)
    (if (good-enough? guess x) 
        guess
        (cbrt-iter (improve guess x) x)))

(define (good-enough? guess x)
    (= (improve guess x) guess))

(define (improve guess x)
    (/  (+   (/ x (square guess)) 
            (* 2 guess))
        3))

(define (square x)
    (* x x))

(define (cbrt x)
    (cbrt-iter 1.0 x))

cbrt -27