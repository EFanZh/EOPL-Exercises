#lang eopl

;; Exercise 2.14 [★★] Extend the representation of the preceding exercise to include a third procedure that implements
;; has-binding? (see exercise 2.9).

(define var-exp
  (lambda (var)
    var))

(define lambda-exp
  (lambda (bound-var body)
    `(lambda (,bound-var)
       ,body)))

(define app-exp
  (lambda (operator operand)
    `(,operator ,operand)))

(define var-exp? symbol?)

(define lambda-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'lambda))))

(define app-exp?
  (lambda (exp)
    (and (pair? exp)
         (not (eqv? (car exp) 'lambda)))))

(define var-exp->var
  (lambda (exp)
    exp))

(define lambda-exp->bound-var caadr)

(define lambda-exp->body caddr)

(define app-exp->rator car)

(define app-exp->rand cadr)

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
