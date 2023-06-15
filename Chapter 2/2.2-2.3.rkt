#lang sicp


(define (square x) (* x x))
(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))
(define (y-point p)
    (cdr p))

(define (make-segment p1 p2)
    (cons p1 p2))

(define (start-segment segment)
    (car segment))
(define (end-segment segment)
    (cdr segment))

(define (seg-len segment)

    (let ((startp (start-segment segment))
          (endp (end-segment segment)))
        (sqrt (+    (square (- (x-point startp) (x-point endp)))
                    (square (- (y-point startp) (y-point endp)))))))

(define (midpoint-segment segment)
    (make-point (/ (+   (x-point (start-segment segment))
                        (x-point (end-segment segment)))
                    2.0)
                (/ (+   (y-point (start-segment segment))
                        (y-point (end-segment segment)))
                    2.0)))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display " , ")
    (display (y-point p))
    (display ")")
    (newline))

(define start (make-point 1 4))
(define end (make-point 2 5))
(define segment (make-segment start end))
(print-point (midpoint-segment segment))

;; 2.3

(define (get-vertical-len rectangle)
    (seg-len (cdr (car rectangle))))

(define (get-horizontal-len rectangle)
    (seg-len (car (car rectangle))))

;; makes a rectangle from points a b c d
;;a     b
;;      
;;d     c
(define (make-rec-from-points a b c d)
    (let (  (ab (make-segment a b))
            (dc (make-segment d c))
            (ad (make-segment a d))
            (bc (make-segment b c)))
        (cons (cons ab dc) (cons ad bc))))


;;makes a rectangle from a point a and x-displacement and y-displacement
(define (make-rec-from-disp a x-disp y-disp)
    (define c (make-point (+ (x-point a) x-disp) (+ (y-point a) y-disp)))
    (define b (make-point (+ x-disp (x-point a)) (y-point a)))
    (define d (make-point (x-point a) (+ (y-point a) y-disp)))
    (let ((ab (make-segment a b))
          (bc (make-segment b c))
          (dc (make-segment d c))
          (ad (make-segment a d)))
            (cons (cons ab dc) (cons ad bc))))

(define (perimeter-rec rectangle)
    (* 2 (+ (get-horizontal-len rectangle) (get-vertical-len rectangle))))

(define (area-rec rec)
    (* (get-horizontal-len rec) (get-vertical-len rec)))


(define rec-p (make-rec-from-points (make-point 1 7)
                                    (make-point 6 7)  
                                    (make-point 6 2)
                                    (make-point 1 2)))

(define rec-d (make-rec-from-disp (make-point 1 7) 5 -5))

( rec-p)
(area-rec rec-d)