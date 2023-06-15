#lang sicp

(define (fringe tree)
    (cond   ((null? tree) nil)
            ((not (pair? tree)) (list tree))
            (else (append (fringe (car tree) ) (fringe (cdr tree))))))

(fringe (list (list 1 2) (list 3 4 (list 54 (list (list (list 8)))))))