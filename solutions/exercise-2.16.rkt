#lang eopl

;; Exercise 2.16 [★] Modify the implementation to use a representation in which there are no parentheses around the
;; bound variable in a lambda expression.

(require "exercise-2.15.rkt")

(define lambda-exp
  (lambda (bound-var body)
    `(lambda ,bound-var ,body)))

(define lambda-exp->bound-var cadr)

(provide var-exp
         lambda-exp
         app-exp
         var-exp?
         lambda-exp?
         app-exp?
         var-exp->var
         lambda-exp->bound-var
         lambda-exp->body
         app-exp->rator
         app-exp->rand)
