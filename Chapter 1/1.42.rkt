#lang sicp

(define (square x)(* x x ))
(define (inc x) (+ x 1))

(define (compose f g)
    (lambda (x)
        (f (g x))))

((compose square inc) 3)

((lambda (x)
    ((lambda (x) (inc (inc x)))
        ((lambda (x) (inc (inc x)))  x))) 2)
