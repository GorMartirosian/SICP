#lang sicp
(#%require sicp-pict)

(define (square x)
    (* x x))

;;Ex. 2.1
(define (make-rat n d)
    (let ((divisor (gcd n d)))
        (let ((numer (/ n divisor))
              (denom (/ d divisor)))
            (if (positive? (* numer denom))
                (cons (abs numer) (abs denom))
                (cons (- (abs numer)) (abs denom))))))

;;Ex. 2.2
(define (make-point x-coord y-coord)
    (cons x-coord y-coord))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(define (make-segment p1 p2)
    (cons p1 p2))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (length-segment seg)
    (let ((startp (start-segment seg))
          (endp (end-segment seg) ))
        (let ((dx (- (x-point startp) (x-point endp)))
              (dy (- (y-point startp) (y-point endp))))
            (sqrt (+ (square dx) (square dy))))))

(define (midpoint-segment seg)
    (let ((x-midpoint (average (x-point (start-segment seg))
                               (x-point (end-segment seg))))
          (y-midpoint (average (y-point (start-segment seg))
                               (y-point (end-segment seg)))))
        (make-point x-midpoint y-midpoint)))

(define (average a b)
    (/ (+ a b) 2))

;;Ex. 2.3
;;angle p1p2p3 is right
(define (make-rectangle p1 p2 p3)
    (let ((d12 (length-segment (make-segment p1 p2)))
          (d23 (length-segment (make-segment p2 p3))))
        (let ((height (max d12 d23)) 
              (width  (min d12 d23)))
            (cons height width))))

(define (rect-width rec)
    (cdr rec))

(define (rect-height rec)
    (car rec))

(define (rect-area rec)
    (* (rect-width rec)
       (rect-height rec)))

(define (rect-perimeter rec)
    (* 2 (+ (rect-height rec)
            (rect-width rec))))

;;Ex. 2.4
;(define (cdr z)
;    (z (lambda (p q) q)))

;;Ex 2.5
(define (cons-nonnegatives a b)
    (* (expt 2 a) (expt 3 b)))

(define (divides? div n)
    (= (remainder n div) 0))

(define (car-nonnegatives n)
    (define (iter a n)
        (if (not (divides? 2 n))
            a
            (iter (inc a) (/ n 2))))
    (iter 0 n))

(define (cdr-nonnegatives n)
    (define (iter b n)
        (if (not (divides? 3 n))
            b
            (iter (inc b) (/ n 3))))
    (iter 0 n))

;;Ex. 2.6 Church Numerals!!!
;(define one (lambda (f)
;                (lambda (x)
;                    (f x))))
;
;(define two (lambda (f)
;                (lambda (x)
;                    (f (f x)))))
;
;(define (add n1 n2)
;    (lambda (f)
;        (lambda (x)
;            ((n1 f) ((n2 f) x)))))

;;Ex. 2.7
(define (make-interval l h)
    (cons l h))
(define upper-bound cdr)
(define lower-bound car)

;;Ex. 2.8
(define (sub-interval a b)
    (make-interval (- (lower-bound a) (upper-bound b))
                   (- (upper-bound a) (lower-bound b))))

;;Ex. 2.10
(define (div-interval x y) 
   (if (<= (* (lower-bound y) (upper-bound y)) 0) 
       (error "Division error (interval spans 0)" y) 
       (mul-interval x  
                     (make-interval (/ 1. (upper-bound y)) 
                                    (/ 1. (lower-bound y))))))

(define (mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4))))

;;Ex. 2.12
(define (make-center-percent center percent)
    (let ((width (* center percent 0.01)))
        (make-interval (- center width) (+ center width))))

(define (get-width interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (center i) (/ (+ (upper-bound i) (lower-bound i)) 2))

(define (percent interval)
    (* (/ (get-width interval) (center interval)) 100))

;;Ex. 2.17
(define (last-pair l)
    (let ((rest (cdr l)))
        (if (null? rest)
            l
            (last-pair rest))))

;;Ex. 2.18
(define (reverse l)
    (define (iter l result)
        (if (null? l)
            result
            (iter (cdr l) (cons (car l) result))))
    (iter l nil))

;;Ex. 2.20
(define (same-parity first . integers)
    (define (matching-parities? x1 x2)
        (equal? (even? x1) (even? x2)))
    (define (helper integers)
        (cond ((null? integers) nil)
              ((matching-parities? first (car integers)) (cons (car integers) (helper (cdr integers))))
              (else (helper (cdr integers) ))))
    (cons first (helper integers)))

;;Ex 2.23
(define (for-each proc l)
    (cond   ((null? l) "Done.")
            (else (proc (car l))
                  (for-each proc (cdr l)))))

;;Ex. 2.27
(define (deep-reverse l)
    (define (iter l result)
        (if (null? l)
            result
            (iter (cdr l) (cons (if (not (pair? (car l))) 
                                    (car l)
                                    (deep-reverse (car l))) result))))
    (iter l nil))

;;Ex. 2.28
(define (fringe tree)
    (cond ((null? tree) '())
          ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
          (else (cons (car tree) (fringe (cdr tree))))))

;;Ex. 2.29
(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

;; a
(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))

(define (is-simple-weight? structure)
    (not (pair? structure)))

;; b
(define (total-weight mobile)
    (if (is-simple-weight? mobile) mobile
        (+  (total-weight (branch-structure (left-branch mobile)))
            (total-weight (branch-structure (right-branch mobile))))))

(define smaller (make-mobile (make-branch 3 4) (make-branch 3 4)))
(define bigger (make-mobile (make-branch 12 smaller) (make-branch 12 8)))

;; c
(define (balanced-mobile? mobile)
    (define (torque branch)
        (* (branch-length branch) (total-weight (branch-structure branch))))
    (if (is-simple-weight? mobile) 
        true
        (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
             (balanced-mobile? (branch-structure (left-branch mobile)))
             (balanced-mobile? (branch-structure (right-branch mobile))))))

;;Ex. 2.30
(define (square-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree-2 tree)
    (map (lambda (sub-tree)
            (if (not (pair? sub-tree)) 
                (square sub-tree)
                (square-tree-2 sub-tree))) 
         tree))

;;Ex. 2.31
(define (tree-map f tree)
    (map (lambda (sub-tree)
            (if (not (pair? sub-tree))
                (f sub-tree)
                (tree-map f sub-tree))) 
         tree))

;;Ex. 2.32
(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest-subsets (subsets (cdr s))))
            (append rest-subsets 
                    (map (lambda (rest-subset-elm)
                            (cons (car s) rest-subset-elm)) 
                         rest-subsets)))))

(define (accumulate op initial seq)
    (if (null? seq) initial
        (op (car seq) (accumulate op initial (cdr seq)))))

;;Ex 2.33
(define (map p sequence)
    (accumulate (lambda (first already-mapped-seq) 
                    (cons (p first) already-mapped-seq)) 
                nil 
                sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length seq)
    (accumulate (lambda (first already-counted) (inc already-counted)) 0 seq))

;;Ex. 2.34
(define (horner-eval x coefficient-seq)
    (accumulate (lambda (this-coeff higher-terms)
                        (+ this-coeff (* x higher-terms))) 
                0
                coefficient-seq))

;;Ex. 2.35
(define (count-leaves tree)
    (accumulate + 0 
                (map (lambda (subtree)
                        (cond ((null? subtree) 0)
                              ((pair? subtree) (count-leaves subtree))
                              (else 1))) 
                    tree)))

;;Ex. 2.36
(define (accumulate-n op initial sequences)
    (cond ((null? sequences) "Empty sequence")
          ((null? (car sequences)) nil)
          (else (cons  (accumulate op initial (map car sequences))
                       (accumulate-n op initial (map cdr sequences))))))

;;Ex. 2.37
(define (get-row n matrix)
    (nth n matrix))

(define (get-column n matrix)
    (map (lambda (row) (nth n row)) matrix))

(define (nth n list)
    (if (= n 0) 
        (car list) 
        (nth (dec n) (cdr list))))

(define (dot-product v w)
    (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
    (map (lambda (row)
            (dot-product row v))  
         m))

(define (transpose m)
    (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (m-row)
                (map (lambda (col)
                        (dot-product m-row col))
                     cols))
             m)))

(define fold-right accumulate)

(define (fold-left op init seq) 
   (if (null? seq) 
       init 
       (fold-left op (op init (car seq)) (cdr seq))))

;;Ex. 2.39
(define (reverse-2 sequence)
    (fold-right (lambda (first already-reversed) 
                    (append already-reversed (list first))) 
                nil 
                sequence))

(define (reverse-3 sequence)
    (fold-left (lambda (to-be-consed-onto first) 
                    (cons first to-be-consed-onto)) 
               nil 
               sequence))

(define (enumerate-interval low high)
    (if (> low high) 
        nil
        (cons low (enumerate-interval (inc low) high))))

(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (!= a b)
    (not (= a b)))

(define (flatmap f seq)
    (accumulate append nil (map f seq)))

;;Ex. 2.40
(define (unique-pairs n)
    (flatmap (lambda (i)
            (map (lambda (j)
                    (list i j))
                 (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))

;;Ex 2.41
(define (sum-of-triples n s)
    (define (all-triples n)
        (flatmap (lambda (i)
            (flatmap (lambda (j)
                   (map (lambda (k)
                            (list i j k))
                        (enumerate-interval 1 n)))
                 (enumerate-interval 1 n)))
         (enumerate-interval 1 n)))
    (filter (lambda (triple)
                (= s (accumulate + 0 triple))) (all-triples n)))

;;Ex. 2.42 8-queens
(define (in-list? val l)
    (accumulate (lambda (this result)
                            (or (= val this) result)) 
                false 
                l))

;Coordinate is in (col row) form.
(define (coord-rests-on-diagonals? coord queen-positions)
    (define get-row cadr)
    (define get-col car)
    (define (coords-on-diagonal? coord1 coord2)
        (= (abs (- (get-row coord1) (get-row coord2)))
           (abs (- (get-col coord1) (get-col coord2)))))
    (accumulate (lambda (current-coord result)
                    (or (coords-on-diagonal? coord current-coord) result))
                false
                queen-positions))

;;Ex. 2.44
(define (up-split painter n)
    (if (= 0 n)
        painter
        (let ((upper-half (up-split painter (- n 1))))
            (below painter (beside upper-half upper-half)))))

(define (right-split painter n)
    (if (= n 0)
        painter
        (let ((right-part (right-split painter (- n 1))))
            (beside painter (below right-part right-part)))))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
          (let ((top-left (beside up up))
                (bottom-right (below right right))
                (corner (corner-split painter (- n 1))))
            (beside (below painter top-left)
                    (below bottom-right corner))))))

;;Ex. 2.45
(define (split comb-picture comb-splitted-pictures)
    (define (f painter n)
        (if (= n 0)
            painter
            (let ((smaller-part (f painter (- n 1))))
                (comb-picture painter
                              (comb-splitted-pictures smaller-part smaller-part)))))
    f)

;;Ex. 2.46
(define (make-vect x y)
    (list x y))

(define (xcor-vect v)
    (car v))

(define (ycor-vect v)
    (cadr v))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))

(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))

;;Ex. 2.48 go to Ex. 2.2

;;Ex. 2.49 a
(define (outline frame)
    (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                             (make-segment (make-vect 1 0) (make-vect 1 1))
                             (make-segment (make-vect 1 1) (make-vect 0 1))
                             (make-segment (make-vect 0 1) (make-vect 0 0)))))

;;b
(define (x-shape frame)
    (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                             (make-segment (make-vect 1 0) (make-vect 0 1)))))

;;Ex. 2.50
(define (flip-horiz2 painter)
    (transform-painter (make-vect 1 0)
                       (make-vect 0 0)
                       (make-vect 1 1)))

(define (rotate180-2 painter)
    (transform-painter painter
                       (make-vect 1 1)
                       (make-vect 0 1)
                       (make-vect 1 0)))

(define (rotate270-2 painter)
    (transform-painter painter
                       (make-vect 0 1)
                       (make-vect 0 0)
                       (make-vect 1 1)))

;;Ex. 2.51
(define (below1 painter1 painter2)
    (let ((split-point (make-vect 0 0.5)))
        (let ((paint-upper (transform-painter painter2 split-point (make-vect 1 0.5) (make-vect 0 1)))
              (paint-lower (transform-painter painter1 (make-vect 0 0) (make-vect 1 0) split-point)))
            (lambda (frame)
                (paint-upper frame)
                (paint-lower frame)))))

(define (below2 painter1 painter2)
    (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(define (square-of-four op1 op2 op3 op4)
    (lambda (painter)
        (beside (below (op3 painter) (op1 painter))
                (below (op4 painter) (op2 painter)))))

(define (square-limit painter n) 
   (let ((combine4 (square-of-four flip-horiz identity 
                                   rotate180 flip-vert))) 
     (combine4 (corner-split painter n))))

;;Ex. 2.54
;(define (equal? l1 l2)
;    (cond ((and (symbol? l1) (symbol? l2)) (eq? l1 l2))
;          ((and (list? l1) (list? l2)) (if (and (null? l1) (null? l2)) 
;                                           true
;                                           (and (equal? (car l1) (car l2)) 
;                                                (equal? (cdr l1) (cdr l2)))))
;          (else false)))

;;Ex. 2.56
(define (deriv exp var) 
   (cond ((number? exp) 0) 
         ((variable? exp) 
            (if (same-variable? exp var) 1 0)) 
         ((sum? exp) 
            (make-sum  (deriv (addend exp) var) 
                       (deriv (augend exp) var))) 
         ((product? exp) 
            (make-sum 
                (make-product (multiplier exp) 
                              (deriv (multiplicand exp) var)) 
                (make-product (deriv (multiplier exp) var) 
                              (multiplicand exp))))
         ((exponentiation? exp) 
            (let ((b (base exp))
                  (e (exponent exp)))
                (make-product (deriv b var)
                              (make-product e (make-exponentiation b (make-sum e -1))))))
         (else 
            (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**)))

(define (base expon)
    (cadr expon))

(define (exponent expon)
    (caddr expon))

(define (make-exponentiation b e)
    (cond ((=number? b 0) 0)
          ((=number? b 1) 1)
          ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))


(define (variable? x) (symbol? x)) 
  
(define (same-variable? v1 v2) 
   (and (variable? v1) (variable? v2) (eq? v1 v2))) 

(define (=number? exp num) 
   (and (number? exp) (= exp num)))

;;Ex. 2.58
(define (min-priority-op exp)
    (cond ((pair? (memq '+ exp)) '+)
          ((pair? (memq '* exp)) '*)
          (else 'no-op)))

(define (sublist l delimiting-symbol)
    (cond ((or (null? l) (eq? (car l) delimiting-symbol)) '())
          ((eq? (car l) delimiting-symbol) '())
          (else (cons (car l) (sublist (cdr l) delimiting-symbol)))))

(define (make-sum a1 a2) 
   (cond ((=number? a1 0) a2) 
         ((=number? a2 0) a1) 
         ((and (number? a1) (number? a2)) (+ a1 a2)) 
         (else (list '+ a1 a2)))) 
  
(define (make-product m1 m2) 
   (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
         ((=number? m1 1) m2) 
         ((=number? m2 1) m1) 
         ((and (number? m1) (number? m2)) (* m1 m2)) 
         (else (list '* m1 m2)))) 
   
(define (sum? exp) 
   (eq? (min-priority-op exp) '+)) 

(define (addend sum-exp)
    (let ((result (sublist sum-exp '+)))
        (if (= 1 (length result))
            (car result)
            result))) 
  
(define (augend s)
    ;;For Ex. 2.57
    ;(define (make-sum-from-list l)
    ;    (cond ((null? l) 0)
    ;          ((null? (cdr l)) (car l))
    ;          (else (make-sum (car l) (make-sum-from-list (cdr l))))))
    ;(make-sum-from-list (cddr s))
    (let ((a2 (cdr (memq '+ s))))
        (if (= 1 (length a2))
            (car a2)
            a2))) 

(define (product? exp) 
   (eq? (min-priority-op exp) '*)) 
  
(define (multiplier p)
    (let ((result (sublist p '*)))
        (if (= 1 (length result))
            (car result)
            result))) 
  
(define (multiplicand p)
    ;;For Ex. 2.57
    ;(define (make-prod-from-list l)
    ;    (cond ((null? l) 1)
    ;          ((null? (cdr l)) (car l))
    ;          (else (make-product (car l) (make-prod-from-list (cdr l))))))
    ;(make-prod-from-list (cddr p))
    (let ((p2 (cdr (memq '* p))))
        (if (= 1 (length p2))
            (car p2)
            p2)))

;;Ex. 2.59
(define (element-of-set? x s)
    (cond ((null? s) false)
          ((equal? (car s) x) true)
          (else (element-of-set? x (cdr s)))))

(define (adjoin-set x s)
    (if (element-of-set? x s) s (cons x s)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
          (else (union-set (cdr set1) (cons (car set1) set2)))))

;;Ex. 2.68 (good one)
(define (encode-symbol sym tree)
    (define (scan-tree tree)
        (cond ((null? tree) false)
              ((leaf? tree) (if (eq? (symbol-leaf tree) sym)
                                '()
                                false))
              ((element-of-set? sym (symbols tree)) (let ((left-result (scan-tree (left-branch2 tree)))
                                                          (right-result (scan-tree (right-branch2 tree))))
                                                        (cond ((not (or left-result right-result)) false)
                                                              (left-result (cons 0 left-result))
                                                              (else (cons 1 right-result)))))
              (else false)))
    (let ((result (scan-tree tree)))
        (if (pair? result) result (error "No symbol" sym "in the tree!"))))
 
(define (make-leaf symbol weight) 
   (list 'leaf symbol weight)) 
(define (leaf? object) 
  (eq? (car object) 'leaf)) 
(define (symbol-leaf x) (cadr x)) 
(define (left-branch2 tree) (car tree)) 
(define (right-branch2 tree) (cadr tree)) 
(define (symbols tree) 
  (if (leaf? tree) 
       (list (symbol-leaf tree)) 
       (caddr tree))) 
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) 
   (list left 
         right 
         (append (symbols left) (symbols right)) 
         (+ (weight left) (weight right))))

(define (weight tree) 
   (if (leaf? tree) 
       (weight-leaf tree) 
       (cadddr tree))) 
  
(define (encode message tree) 
   (if (null? message) 
       '() 
       (append (encode-symbol (car message) tree) 
               (encode (cdr message) tree))))
(define sample-tree
(make-code-tree (make-leaf 'A 4)
(make-code-tree
(make-leaf 'B 2)
(make-code-tree
(make-leaf 'D 1)
(make-leaf 'C 1)))))

;;Ex. 2.69
(define (successive-merge leaves)
    (cond ((null? leaves) '())
          ((null? (cdr leaves)) (car leaves))
          (else (successive-merge (adjoin-set (make-code-tree (car leaves) (cdr leaves))
                                              (cddr leaves))))))

;;Ex. 2.73 b.
(define (deriv-sum-package-installer)
    (define addend car)
    (define augend cdar)
    (define (make-sum op1 op2)
        (list '+ op1 op2))

    ;;interface to the deriv proc
    (define (deriv-sum operands var)
        (make-sum (deriv (addend operands) var)
                  (deriv (augend operands) var)))

    ;(put 'make-sum '(+) make-sum)
    ;(put 'deriv '(+) deriv-sum)
    ;;bottom expression is intended to not disturb the compiler
    (+ 2 2)
    )

(define (deriv-prod-package-installer)
    (define multiplier car)
    (define multiplicand cdar)
    (define (make-prod op1 op2)
        (list '* op1 op2))

    ;;interface to the deriv proc
    (define (deriv-prod operands var)
        ;;these version of make-sum is supposed to be defined here
        (make-sum (make-product (deriv (multiplier operands) var) (multiplicand operands))
                  (make-product (deriv (multiplicand operands) var) (multiplier operands))))

    ;(put 'make-prod '(*) make-prod)
    ;(put 'deriv '(*) deriv-prod)
    (+ 2 2)
    )

;; c
(define (deriv-exp-package-installer)
    (define base car)
    (define exponent cdar)
    (define (make-exponent base exp)
        (list '** base exp))

    ;;interface to the deriv proc
    (define (deriv-exp operands var)
        ;;these version of make-sum is supposed to be defined here, but if needed can be used from the
        ;;existing codebase of the deriv generic selector-type table
        (make-product (exponent operands)
                      (make-exponent (base operands) (make-sum (exponent operands) -1))))

    ;(put 'make-exponent '(**) make-exponent)
    ;(put 'deriv '(**) deriv-exp)
    (+ 2 2)
    )

;;Ex. 2.75
(define (make-from-mag-and r ang)
    (define (dispatch op)
        (cond ((eq? op 'real-part) (* r (cos ang)))
              ((eq? op 'imag-part) (* r (sin ang)))
              ((eq? op 'magnitude) r)
              ((eq? op 'angle) ang)
              (else (error "Unknown op: " op))))
    dispatch)