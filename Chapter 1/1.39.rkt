#lang sicp

(define (square x) (* x x))

(define (cont-frac n d k)
    (define (iter i)
        (if (> i k) 0
            (/  (n i) 
                (+  (d i) 
                    (iter (+ 1 i))))
        )
    )
    (iter 1))


(define (tan-cf x k)
    (define (n-lambert index)
        (if (= index 1)
            x
            ( - (square x))))
    (define (d-lambert index)
        (+ 1 (* 2 (- index 1))))
    (cont-frac n-lambert d-lambert k)
    )

(tan-cf (/ 3.141592 6) 100000)