#lang sicp

;;returns a procedure which applies the arg twice
(define (double func-with-one-arg)
    (lambda (arg) 
        (func-with-one-arg (func-with-one-arg arg)))
)

(define (inc x)( + x 1))

(define inc-by-two (double inc))
(inc-by-two 2)