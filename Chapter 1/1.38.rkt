#lang sicp

(define (cont-frac n d k)
    (define (iter i)
        (if (> i k) 0
            (/  (n i) 
                (+  (d i) 
                    (iter (+ 1 i))))
        )
    )
    (iter 1))

(define (denom-euler index)
    (cond ((= 0 (remainder (- index 2) 3)) (* 2 (+ (/ (- index 2) 3) 1)))
          (else 1)))

(+ 2 (cont-frac (lambda (i) 1.0) denom-euler 10000))