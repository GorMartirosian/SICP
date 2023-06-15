#lang sicp

(define (for-each proc lst)
    (cond   ((not (null? lst))
            (proc (car lst))
            (for-each proc (cdr lst)))
    ))

(define (pr x) (display (* 2 x)))
(for-each pr (list 1 2 3))