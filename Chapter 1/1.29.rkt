#lang sicp

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b dx)
    (define (next-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) next-dx b) dx))

(define (simp-sum term a b next n)
    (define (iter a i)
        (define (coef)
            (cond   ((or (= i 0) (= i n)) 1)
                    ((= (remainder i 2) 0) 2)
                    (else 4)))

        (if (> a b)
            0
            (+ (* (coef) (term a) ) 
                (iter (next a) (+ i 1))))
    )
    (iter a 0)
)

(define (simpson-integral f a b n)
    (define (h) (/ (- b a) n))
    (define (next x) (+ x (h)))
    (* (/ (h) 3.0) (simp-sum f a b next n)))

(simpson-integral cube 0 3 1000)