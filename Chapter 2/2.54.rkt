#lang sicp

(define (equal? list1 list2)
    (cond   ((and (null? list1) (null? list2)) true)
            ((or (null? list1) (null? list2)) false)     
            ((and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2))
            ((and (pair? list1) (pair? list2)) (and (equal? (car list1) (car list2))
                                                    (equal? (cdr list1) (cdr list2))))
            (else false)))


(equal? (list 'a (list 1 '1 4 '())) (list 'a (list 1 1 4 '())))

(equal? 1 (list 1))

