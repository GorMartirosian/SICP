#lang sicp

(define (first-denomination coins)
    (car coins))

(define (no-more? coins)
    (null? coins))
(define (except-first-denomination coins)
    (cdr coins))