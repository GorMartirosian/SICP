#lang sicp

(define (func x y)
    (define (dispatch op)
        (cond ((eq? op 'real) x)))
    dispatch)

(define obj (func 4 5))

(define (apply-generic op arg)
    (arg op))

(define (make-from-mag-ang mag ang)
    (define (dispatch op)
        (cond ((eq? op 'real-part) (* mag (cos ang)))
              ((eq? op 'imag-part)  (* mag (sin ang)))
              ((eq? op 'magnitude) mag)
              ((eq? op 'angle) ang)
              (else (error "Undefined operation " op))))
    dispatch)


