#lang racket

(require rackunit)
(require "../solutions/exercise-2.28.rkt")

(check-equal? (unparse-lc-exp (var-exp 'a)) "a")
(check-equal? (unparse-lc-exp (lambda-exp 'a (var-exp 'b))) "(lambda (a) b)")
(check-equal? (unparse-lc-exp (app-exp (var-exp 'a) (var-exp 'b))) "(a b)")
(check-equal? (unparse-lc-exp (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'c)))) "(lambda (a) (b c))")

(check-equal? (unparse-lc-exp (app-exp (app-exp (var-exp 'a) (var-exp 'b)) (app-exp (var-exp 'c) (var-exp 'd))))
              "((a b) (c d))")
