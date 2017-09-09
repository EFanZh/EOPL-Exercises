#lang racket

(require rackunit)
(require "exercise-2.15.rkt")

(let ([var-exp-example (var-exp 'a)]
      [lambda-exp-example (lambda-exp 'a (var-exp 'b))]
      [app-exp-example (app-exp (var-exp 'a) (var-exp 'b))])
  (check-true (var-exp? var-exp-example))
  (check-false (var-exp? lambda-exp-example))
  (check-false (var-exp? app-exp-example))
  (check-false (lambda-exp? var-exp-example))
  (check-true (lambda-exp? lambda-exp-example))
  (check-false (lambda-exp? app-exp-example))
  (check-false (app-exp? var-exp-example))
  (check-false (app-exp? lambda-exp-example))
  (check-true (app-exp? app-exp-example))
  (check-eqv? (var-exp->var var-exp-example) 'a)
  (check-eqv? (lambda-exp->bound-var lambda-exp-example) 'a)
  (check-eqv? (var-exp->var (lambda-exp->body lambda-exp-example)) 'b)
  (check-eqv? (var-exp->var (app-exp->rator app-exp-example)) 'a)
  (check-eqv? (var-exp->var (app-exp->rand app-exp-example)) 'b))
