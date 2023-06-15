#lang sicp


(define (all-odds lst)
    (if (null? lst)
        nil
        (if (odd? (car lst)) 
            (cons (car lst) (all-odds (cdr lst)))
            (all-odds (cdr lst)))))

(define (all-evens lst)
    (if (null? lst)
        nil
        (if (even? (car lst)) 
            (cons (car lst) (all-evens (cdr lst)))
            (all-evens (cdr lst)))))

(define (same-parity first . others)
    (if (odd? first)
        (cons first (all-odds others))
        (cons first (all-evens others))))

(same-parity 1 3)