#lang sicp

;;Ex. 4.1
(define (list-of-values-r exps env)
    (if (null? exps)
        '()
        (let ((rest (list-of-values-r (cdr exps) env)))
            (cons (eval (car exps) env)
                  rest))))

(define (list-of-values-l exps env)
    (if (null? exps)
        '()
        (let ((first (eval (car exps) env)))
            (cons first
                  (list-of-values-l (cdr exps) env)))))