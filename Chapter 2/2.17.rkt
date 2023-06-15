#lang sicp

(define (last-pair list)
    (if (null? (cdr list))
        (car list)
        (last-pair (cdr list))))


(last-pair (list 1 2 3 4 4 3))