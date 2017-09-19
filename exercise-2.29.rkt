#lang eopl

;; Exercise 2.29 [★] Where a Kleene star or plus (page 7) is used in concrete syntax, it is most convenient to use a
;; list of associated subtrees when constructing an abstract syntax tree. For example, if the grammar for
;; lambda-calculus expressions had been
;;
;;     Lc-exp ::= Identifier
;;                ┌───────────────┐
;;                │ var-exp (var) │
;;                └───────────────┘
;;            ::= (lambda ({Identifier}∗) Lc-exp)
;;                ┌──────────────────────────────┐
;;                │ lambda-exp (bound-vars body) │
;;                └──────────────────────────────┘
;;            ::= (Lc-exp {Lc-exp}∗)
;;                ┌───────────────────────┐
;;                │ app-exp (rator rands) │
;;                └───────────────────────┘
;;
;; then the predicate for the bound-vars field could be (list-of identifier?), and the predicate for the rands field
;; could be (list-of lc-exp?). Write a define-datatype and a parser for this grammar that works in this way.

(define identifier?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? x 'lambda)))))

(define-datatype lc-exp lc-exp?
  [var-exp [var identifier?]]
  [lambda-exp [bound-vars (list-of identifier?)]
              [body lc-exp?]]
  [app-exp [rator lc-exp?]
           [rands (list-of lc-exp?)]])

(define parse-expression
  (lambda (datum)
    (cond [(identifier? datum) (var-exp datum)]
          [(pair? datum) (if (eqv? (car datum) 'lambda)
                             (lambda-exp (cadr datum)
                                         (parse-expression (caddr datum)))
                             (app-exp (parse-expression (car datum))
                                      (map parse-expression (cdr datum))))]
          [else (eopl:error 'parse-expression "Invalid expression: ~s" datum)])))

(provide cases lc-exp var-exp lambda-exp app-exp parse-expression)
