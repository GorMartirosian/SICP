#lang sicp
(define (f-rec n)
    (if (< n 3)
        n
        (+  (f-rec (- n 1)) 
            (* 2 (f-rec (- n 2))) 
            (* 3 (f-rec (- n 3))))))

(define (f-iter-helper a b c count)
    (if (= count 0)
        a
        (f-iter-helper b c (+   c
                                (* 2 b)
                                (* 3 a))
                                (- count 1))))


(define (f-iter n)
    (if (< n 3) 
        n
        (f-iter-helper 0 1 2 n)))

(f-iter 4)