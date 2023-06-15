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

(define (cont-frac-iter n d k)
    (define (iter i result)
        (if(= i 0)
            result
            (iter   (- i 1)
                    (/ (n i) (+ (d i) result)))
        )
    )
    (iter k 0))


(cont-frac (lambda (i) 1.0)
        (lambda (i) 1.0)
        100000)
(cont-frac-iter (lambda (i) 1.0)
        (lambda (i) 1.0)
        100000)