#lang sicp

(define (reverse list)

    (define (opposite n)
        (if (= n 0)
            (cons (list-ref list 0) nil)
            (cons (list-ref list n) (opposite (- n 1))))
    )
    (opposite (- (length list) 1))
)

(reverse (list 1 2 4 6))