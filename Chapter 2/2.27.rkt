#lang sicp


(define (deep-reverse tree)
    (cond   ((null? tree) nil)
            ((pair? tree)
                (append  (deep-reverse (cdr tree)) 
                        (list (deep-reverse (car tree)))))
    (else  tree)))


(deep-reverse (list 1 4))

;; should be (5 22 (2 1))