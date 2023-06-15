#lang sicp

(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s)) ))
            (append rest (map (lambda (current)
                                    (append current (list (car s))) ) rest)))))
                        

(subsets (list 1 2 3))