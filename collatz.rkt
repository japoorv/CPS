#lang racket
;; Sample Problem for search.rkt : Collatz Conjecture Problem "3n+1 Problem" ;; 
(require "search.rkt")

(define (operator_applier op state)
  ((cdr op) state)
  )

(define (div x)
  (cond
    [(and (integer? x) (even? x)) (/ x 2)]
    [else x]
    ))
(define (3n+1 x)
  (cond
    [(and (integer? x) (odd? x)) (+ 1 (* 3 x))]
    [else x]
    ))
(define myProblem (problem "collatz" (lambda (x) (equal? x 1)) equal? print 'no (list (cons 'div div) (cons 'sub 3n+1)) operator_applier))
(define ans (path-so-far (bsolve myProblem 55)))
(println ans)
(println (length ans))