#lang sicp

(define (filtered-accumulate combiner null-value term a next b filter?)
    (define (iter a result)
        (if (> a b)
            result
            (if (filter? (term a))
                (iter (next a) (combiner (term a) result))
                (iter (next a) result))
            ))
    (iter a null-value))

(define (term x) (* x x))
(define (next x) (+ x 1))
(filtered-accumulate + 0 term 0 next 6 even?)