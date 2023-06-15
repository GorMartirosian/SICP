#lang sicp

(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car(cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))


(define (total-weight structure)
    (cond   ((null? structure) 0)
            ((not (pair? structure)) structure)
    (else (+    (total-weight (branch-structure (left-branch structure)))
                (total-weight (branch-structure (right-branch structure)))))))

(define lilmob (make-mobile (make-branch 2 4.0) (make-branch 2 4.0)))
(define mob (make-mobile (make-branch 2 lilmob) (make-branch 4 4.0)))


(total-weight mob)

(define (balanced? mobile)
    (cond   ((null? mobile) true)
            ((not (pair? mobile)) true) 
            (else (and (=   (*      (branch-length (left-branch mobile)) 
                                    (total-weight (branch-structure (left-branch mobile))))
                            (*      (branch-length (right-branch mobile)) 
                                    (total-weight (branch-structure (right-branch mobile)))))
                        (balanced? (branch-structure (left-branch mobile)))
                        (balanced? (branch-structure (right-branch mobile)))
                                ))))

(balanced? mob)