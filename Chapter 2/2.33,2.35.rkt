#lang sicp

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))


(define (map f sequence)
    (accumulate (lambda (elem1 elem2)
                    (cons (f elem1) elem2)) 
                nil 
                sequence))


(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y))
                0 
                sequence))

(define (filter predicate? sequence)
    (cond   ((null? sequence) nil)
            ((predicate? (car sequence)) (cons  (car sequence) 
                                                (filter predicate? (cdr sequence))))
            (else (filter predicate? (cdr sequence)))))
(define (is-leave node)
        (and    (not (pair? node))
                (not (null? node))))
(define (enum-leaves tree)
        (cond   ((null? tree) nil)
                ((is-leave tree) (list tree))
                (else (append (enum-leaves (car tree)) (enum-leaves (cdr tree)) ))))

(define (count-leaves tree)
    (accumulate (lambda (x y) (+ 1 y)) 
                0 
                (enum-leaves tree)))

(enum-leaves (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
(define (enumerate-interval low up)
    (if (> low up) nil 
        (append (list low) (enumerate-interval (+ 1 low) up))))

(define (a-func n)
    (map    (lambda (i)
                (map    (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (generate-list n) (list n (+ n 1)))
(flatmap generate-list (list 1 2 3 ) )

(flatmap a-func (enumerate-interval 1 3) )