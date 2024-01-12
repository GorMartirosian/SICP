#lang sicp

;;Ex. 1.2
;(/  (+ 5 
;       4
;       (- 2 (- 3 (+ 6 (/ 4 5)))))
;    (* 3 (- 6 2) (- 2 7)))


(define (square x)
    (* x x))

(define (sum-of-squares a b)
    (+ (square a) (square b)))

;;Ex. 1.3
(define (sum-of-greater-two a b c)
    (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
          ((and (>= a b) (>= c b)) (sum-of-squares a c))
          (else (sum-of-squares b c))))

(define (average x1 x2)
  (/ (+ x1 x2) 2))

(define (improve guess x)
  (average guess (/ x guess)))

;;Ex. 1.7
(define (good-enough? guess x)
  (= guess (improve guess x)))

;;Ex. 1.8
(define (cbrt-iter guess x)
  (if (good-enough-cbrt? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x) x)))

(define (good-enough-cbrt? guess x)
  (= guess (improve-cbrt guess x)))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt x) (cbrt-iter 1.0 x))

;;Ex. 1.11
(define (f-rec  n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-iter-helper i a b c)
    (if (= i 0) 
        c
        (f-iter-helper (- i 1)
                       b
                       c
                       (+ c (* 2 b) (* 3 a)))))
  (if (< n 3)
      n
      (f-iter-helper (- n 2) 0 1 2)))


;;Ex. 1.12
(define (pascal-triangle row col)
  (if (or (= col 0) (= row col)) 1
      (+ (pascal-triangle (- row 1)
                          (- col 1))
         (pascal-triangle (- row 1)
                          col))))

;;Ex. 1.16
(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b)
                                     (/ n 2)
                                     a))
          (else (fast-expt-iter b
                                (- n 1)
                                (* a b)))))
  (fast-expt-iter b n 1))

(define (halve x)
  (/ x 2))

(define (double x)
  (* 2 x))

;;Ex. 1.17
(define (mult-rec a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult-rec a (halve b))))
        (else (+ a (mult-rec a (- b 1))))))

;;Ex. 1.18
(define (mult a b)
  (define (mult-iter a b mul)
    (cond ((= b 0) mul)
          ((even? b) (mult-iter (double a)
                                (halve b)
                                mul))
          (else (mult-iter a
                           (- b 1)
                           (+ mul a)))))
  (mult-iter a b 0))

;;Ex. 1.20
;;Hint: Create a table of remainder operations contained in each function call
;;                  a b
;;  GCD(206, 40) -> 0 0
;;  GCD(40, 6)   -> 0 1
;;  GCD(4, 2)    -> 1 2
;;  ...          -> 2 4
;;  ...          -> 4 7
;;  2             finished here
;;
;; Look where the a or b is evaluated and add to the sum.
;; Answer: 18 times evaluated remainder

;;Ex. 1.22
(define (search-for-primes start end)
  (define (iter start end)
    (cond ((> start end) (newline))
          ((prime? start) (timed-prime-test start)
                          (iter (+ start 1) end))
          (else (iter (+ start 1) end))))
  (if (even? start) 
      (iter (+ start 1) end)
      (iter start end)))

(define (prime? n)
  (define (helper i)
    (cond ((> (square i) n) true)
          ((= (remainder n i) 0) false)
          (else (helper (+ i 1)))))
  (if (<= n 1) false (helper 2)))

(define (timed-prime-test n)
  (define (start-prime-test start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display "***")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test (runtime)))

(define (sum term a next b)
  (if (> a b)
      0
      (+  (term a)
          (sum term (next a) next b))))

;;Ex. 1.29
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y i)
    (f (+ a (* i h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (/ h 3.0)
     (sum term 0 inc n)))

(define (integral-2 f a b dx)
  (define (term x)
    (f (+ x (/ dx 2.0))))
  (define (next a)
    (+ a dx))
  (* dx (sum term a next b)))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx 
     (sum f (+ a (/ dx 2.0)) add-dx b)))

(define (cube x)
  (* x x x ))

;;Ex 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;Ex. 1.31 a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approx)
  (define up-to 1001)
  (/ (* 8 (product (lambda (a) (/ (square (inc a))
                                               (square a))) 
                                3.0 
                                (lambda (x) (+ x 2))
                                up-to)) (inc up-to)))

;;Ex. 1.31 b
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;;Ex. 1.32 a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum-accum term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-accum term a next b)
  (accumulate * 1 term a next b))

;;Ex. 1.32 b
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;;Ex. 1.33
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

;;a
(define (sum-prime-squares a b)
  (filtered-accumulate + prime? 0 square a inc b))

;;b
(define (product-mutually-primes n)
  (filtered-accumulate * (lambda (x) (= (gcd x n) 1)) 1 identity 2 inc (dec n)))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;Ex. 1.37 a
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) 
           (+ (d i) (rec (inc i))))))
  (rec 1))

;;b
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (dec i) (/ (n i) (+ (d i) result)))))
  (iter (dec k) (/ (n k) (d k))))

;;Ex. 1.38
(define E (+ (cont-frac-iter (lambda (i) 1.0)
                             (lambda (i)
                              (if (= 2 (remainder i 3))
                                  (* (inc (floor (/ i 3)))
                                     2)
                                  1))
                             1000) 
             2))

;;Ex. 1.39
(define (tan-cf x k)
  (let ((squared-x (square x)))
      (cont-frac-iter (lambda (i) (if (= i 1) x (- squared-x)))
                      (lambda (i) (dec (* 2 i)))
                      k)))

;;Ex. 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;Ex 1.41
(define (double-apply f)
  (lambda (x) (f (f x))))

;;Ex. 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;;Ex. 1.43
(define (repeated f n)
  (define (iter i composition)
    (if (= i n)
        composition
        (iter (inc i) (compose f composition))))
  (iter 1 f))

;;Ex. 1.44
(define (smooth f)
  (define dx 0.000001)
  (define (average a b c)
    (/ (+ a b c) 3))
  (lambda (x) (average (f (- x dx))
                       (f x)
                       (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;Ex. 1.45
(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n))) (lambda (y) (/ x (expt y (dec n))))) 1.0))

(define (log2 x)
  (/ (log x) (log 2)))

;;Ex. 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess) 
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt-iterative-improve x)
  (define tolerance-quotient 0.0001)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (* guess tolerance-quotient)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point-iter-improve f first-guess)
  (define tolerance 0.0001)
  ((iterative-improve (lambda (guess) 
                        (< (abs (- (f guess) guess)) tolerance))
                      f) 
   first-guess))