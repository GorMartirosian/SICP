#lang sicp

;;Ex. 3.1
(define (make-accumulator sum)
    (lambda (amount)
        (set! sum (+ sum amount))
        sum))

;;Ex. 3.2
(define (make-monitored f)
    (let ((call-count 0))
        (lambda (x)
            (cond ((eq? x 'how-many-calls?) call-count)
                  ((eq? x 'reset-count) (set! call-count 0)
                                        call-count)
                  (else (set! call-count (inc call-count))
                        (f x))))))

;;Ex 3.3
(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                    balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch entered-pass m)
        (if (eq? entered-pass password) 
            (cond  ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request: MAKE-ACCOUNT" m)))
            (error "Incorrect password!")))
    dispatch)

;;Ex. 3.6
;(define rand
;    (let ((current-random-val 2))
;        (lambda (command)
;            (cond ((eq? command 'reset) (lambda (value)
;                                            (set! current-random-val value)
;                                            "New seed is set."))
;                  ((eq? command 'generate) (set! current-random-val (random-update current-random-val))
;                                            current-random-val)))))

;;Ex. 3.7
(define (make-joint acc1 pass1 pass2)
    (if (acc1 pass1 'withdraw)    ;;check existance of arbitrary method
        (lambda (pass message)
            (cond ((eq? pass pass2) (acc1 pass1 message))
                  (else (error "Password incorrect for joint account: " acc1))))
        (error "Password incorrect for account: " acc1)))

;;Ex. 3.8
(define f (let ((a 0))
            (lambda (x)
                (if (= x 0)
                    (begin (set! a -1)
                           0)
                    (+ a x)))))

;;Ex. 3.17
(define (count-pairs x)
    (let ((checked-cells '()))
        (define (helper x)
            (cond ((not (pair? x)) 0)
                  ((memq x checked-cells) 0)
                  (else (set! checked-cells (cons x checked-cells))
                        (+ 1
                           (helper (car x))
                           (helper (cdr x))))))
        (define (contains? elm cells)
            (cond ((null? cells) false)
                  ((eq? elm (car cells)) true)
                  (else (contains? elm (cdr cells)))))
        (helper x)))

(define (last-pair x)
    (cond ((null? x) '())
          ((null? (cdr x)) x)
          (else (last-pair (cdr x)))))

(define w (list 1 2 3))
(set-cdr! (last-pair w) (cdr w))

;;Ex. 3.18
(define (has-loop? l)
    (let ((checked-cells '()))
        (define (helper node)
            (cond ((not (pair? node)) false)
                  ((memq node checked-cells) true)
                  (else (set! checked-cells (cons node checked-cells))
                        (helper (cdr node)))))
        (helper l)))

;;Ex. 3.19
(define (contains-cycle? lst) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter a b) 
     (cond  ((not (pair? b)) #f) 
            ((eq? a b) #t)  
            (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
   (iter lst (safe-cdr lst)))