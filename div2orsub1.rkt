#lang racket
;; Sample Problem for search.rkt ;;
(require "search.rkt")

(define (operator_applier op state)
  ((cdr op) state)
  )

(define div
  (lambda (x) (/ x 2)))
(define sub
  (lambda (x) (- x 1)))
(define myProblem (problem "div2orsub1" (lambda (x) (equal? x 1)) equal? print 'no (list (cons 'div div) (cons 'sub sub)) operator_applier))
(path-so-far (bsolve myProblem 1000))