#lang sicp

(define (make-accumulator init)
    (define (accum val)
        (begin  (set! init (+ init val))
                init))
    accum)

(define x (make-accumulator 100))

((make-accumulator 100) 20)
((make-accumulator 100) 20)