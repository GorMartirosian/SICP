#lang sicp

(define (double x)(+ x x))
(define (halve x)(/ x 2))

(define (* a b)
    (if (= a 0)
        0
        (mult-iter a b 0)))

(define (mult-iter a b acc)
    (cond   ((= b 0) acc)
            ((even? b) (mult-iter (double a) (halve b) acc)) 
            (else (mult-iter a (- b 1) (+ acc a)))))

(* 3 3)