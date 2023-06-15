#lang sicp

(define (set-last-cdr! lst x)
    (cond   ((null? lst) x)
            ((null? (cdr lst)) (set-cdr! lst x))
            (else (set-last-cdr! (cdr lst) x))))

(define (count-pairs x)
    (define (is-counted? x counted-list)
        (cond ((null? counted-list) false)
              ((eq? (car counted-list) x) true)
              (else (is-counted? x (cdr counted-list)))))

    (define (helper x counted-list)
        (cond   ((or (null? x) (not (pair? x))) 0)
                ((is-counted? x counted-list) (+ (helper (car x) counted-list) (helper (cdr x) counted-list)))
                (else (begin (set-last-cdr! counted-list (list x))
                             (+  1 
                                (helper (car x) counted-list)
                                (helper (cdr x) counted-list))))))
    (helper x '()))

(count-pairs (list 1 2 3 (list 1 2 2)))

((lambda ( a b) (+ a b)) 4 8)
