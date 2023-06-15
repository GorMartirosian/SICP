#lang sicp

(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))
(define (term x) (/ 1 x))
(define (next x) (+ x 1))
(define (identity x) x)

(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (factorial n)
    (define (next x) (+ x 1))
    (product-iter identity 1 next n))

(factorial 4)