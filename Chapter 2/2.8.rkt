#lang sicp

(define (make-interval low high)
    (cons low high))
(define (lower-bound x)
    (car x))
(define (upper-bound x)
    (cdr x))

(define (sub-interval i1 i2)
    (make-interval  (- (lower-bound i1 ) (upper-bound i2))
                    (- (upper-bound i1) (lower-bound i2))))

(define (center interval)
    (/ (+ (lower-bound interval) (upper-bound interval) ) 2.0))

(define (percent interval)
    (/  (* 100.0 (- (upper-bound interval) (center interval)))
        (center interval)))


(define (make-center-percent center percent)
    (let ((upper (+ center (/ (* center percent) 100.0)))
         (lower (- center (/ (* center percent) 100.0))))
         (make-interval lower upper)
    )
)

(make-center-percent 8 10)