#lang sicp

(define (make-monitored f)
    (let ((counter 0))
        (define (mf message)
            (cond   ((eq? message 'how-many-calls?) counter)
                    ((eq? message 'reset-count) (set! counter 0))
                    (else (set! counter (+ counter 1))
                        (f message))))
        mf))

(define mon (make-monitored sqrt))
(mon 100)
(mon 10000)
(mon 169)
(mon 'how-many-calls?)
(mon 'reset-count)
(mon 100)
(mon 'how-many-calls?)