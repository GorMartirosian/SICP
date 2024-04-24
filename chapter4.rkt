#lang sicp

;;Ex. 4.1
(define (list-of-values-r exps env)
  (if (null? exps)
      '()
      (let ((rest (list-of-values-r (cdr exps) env)))
        (cons (eval (car exps) env)
              rest))))

(define (list-of-values-l exps env)
  (if (null? exps)
      '()
      (let ((first (eval (car exps) env)))
        (cons first
              (list-of-values-l (cdr exps) env)))))

(define (require p)
  (if (not p) (amb)))

;;Ex. 4.35
(define (an-integer-between a b)
  (require (<= a b))
  (amb a
       (an-integer-between (+ a 1) b)))

(define (member? item items)
  (cond ((null? items) false)
        ((equal? item (car items)) true)
        (else (member? item (cdr items)))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member? (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (accumulate init op ls)
  (cond ((null? ls) init)
        (else (op (car ls) (accumulate init op (cdr ls))))))

(define (!= a b) (not (= a b)))

;;Ex. 4.41 (very ugly backtracking solution)
(define (multiple-dwelling)
  (define (perfect-case? baker fletcher smith cooper miller)
    (and (distinct? (list baker fletcher smith cooper miller))
         (> miller cooper)
         (!= (abs (- smith fletcher)) 1)
         (!= (abs (- fletcher cooper)) 1)))
  (define (make-solution baker fletcher smith cooper miller) (list (list 'baker baker)
                                                                   (list 'fletcher fletcher)
                                                                   (list 'smith smith)
                                                                   (list 'cooper cooper)
                                                                   (list 'miller miller)))
  (define (backtrack-search all-possibilities current-combo)
    (cond ((null? all-possibilities) (if (apply perfect-case? current-combo)
                                         (apply make-solution current-combo)
                                         false))
          (else (let ((current-floors (car all-possibilities)))
                  (if (null? current-floors)
                      false
                      (let ((result (backtrack-search (cdr all-possibilities)
                                                      (append current-combo
                                                              (list (car current-floors))))))
                        (if result
                            result
                            (backtrack-search (cons (cdr current-floors)
                                                    (cdr all-possibilities))
                                              current-combo))))))))
  (backtrack-search '((1 2 3 4)
                      (2 3 4)
                      (1 2 3 4 5)
                      (2 3 4 5)
                      (3 4 5)) '()))

;;Ex. 4.42
(define (five-schoolgirls)
  (define (solution b e j k m)
    (list (list 'betty b)
          (list 'ethel e)
          (list 'joan j)
          (list 'kitty k)
          (list 'marry m)))
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (marry (amb 1 2 3 4 5))
        (betty-claim (amb (lambda (b e j k m) (= k 2))
                          (lambda (b e j k m) (= b 3))))
        (ethel-claim (amb (lambda (b e j k m) (= e 1))
                          (lambda (b e j k m) (= j 2))))
        (joan-claim (amb (lambda (b e j k m) (= j 3))
                         (lambda (b e j k m) (= e 5))))
        (kitty-claim (amb (lambda (b e j k m) (= k 2))
                          (lambda (b e j k m) (= m 4))))
        (marry-claim (amb (lambda (b e j k m) (= m 4))
                          (lambda (b e j k m) (= b 1)))))
    (define (all-true? l)
      (if (member? #f l)
          #f
          #t))
    (require  (all-true? (map (lambda (fn)
                                (fn betty ethel joan kitty marry))
                              (list betty-claim
                                    ethel-claim
                                    joan-claim
                                    kitty-claim
                                    marry-claim))))
    (require (distinct? (list betty ethel kitty joan marry)))

    (solution betty ethel joan kitty marry)))

(define (filter pred seq)
    (cond ((null? seq) '())
          ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
          (else (filter pred (cdr seq)))))

;;Ex. 4.44 Eight Queens wiht Amb
(define (queens n)
  (define (cell x y) (list x y))
  (define (x-coord c) (car c))
  (define (y-coord c) (cadr c))
  (define (attack-each-other? q1 q2)
    (or (= (x-coord q1) (x-coord q2))
        (= (y-coord q1) (y-coord q2))
        (= (abs (- (x-coord q1) (x-coord q2)))
           (abs (- (y-coord q1) (y-coord q2))))))
  (define (any-queen-attacks? my-queen queen-list)
    (not (null? (filter (lambda (q)
                     (attack-each-other? q my-queen))
                   queen-list))))
  (define (iter col queens)
    (cond ((> col n) queens)
          (else (let ((this-queen (cell (an-integer-between 1 n) col)))
                    (require (not (any-queen-attacks? this-queen queens)))
                    (iter (inc col) (append queens (list this-queen)))))))
   (iter 1 '()))