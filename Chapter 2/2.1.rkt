#lang sicp

(define (make-rat num denom)
    (if (= denom 0)
        (error "Zero divizion error!"))
    (let ((reduced-num (/ num (gcd num denom)))
          (reduced-denom (/ denom (gcd num denom))))
        (cond  ((and (>= reduced-num 0) (> reduced-denom 0))
                    (cons reduced-num reduced-denom))
                ((and (< reduced-num 0) (< reduced-denom 0))
                    (cons (- reduced-num) (- reduced-denom)))
                ((< reduced-denom 0)
                            (cons (- reduced-num) (- reduced-denom)))
                ((< reduced-num 0)
                    (cons reduced-num reduced-denom)))))
(define (print-rat n)
    (newline)
    (display (car n))
    (display "/")
    (display (cdr n)))


(print-rat (make-rat -3 0))
