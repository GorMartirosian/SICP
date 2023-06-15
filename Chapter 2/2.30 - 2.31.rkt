#lang sicp

(define (square x) (* x x))

(define (square-tree tree)
    (cond   ((null? tree) nil)
            ((not (pair? tree)) (square tree))
            (else (cons (square-tree (car tree)) 
                        (square-tree (cdr tree))))))

(define (square-tree-map tree)
    (map    (lambda (subtree)
                (if (not (pair? subtree)) (square subtree)
                    (square-tree-map subtree))) 
            tree))

(define (tree-map f tree)
    (map    (lambda (subtree)
                (if (not (pair? subtree)) (f subtree)
                    (tree-map f subtree))) 
            tree))

(tree-map square
(list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))