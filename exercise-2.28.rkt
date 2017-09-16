#lang eopl

;; Exercise 2.28 [★] Write an unparser that converts the abstract syntax of an lc-exp into a string that matches the
;; second grammar in this section (page 52).

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  [var-exp [var identifier?]]
  [lambda-exp [bound-var identifier?]
              [body lc-exp?]]
  [app-exp [rator lc-exp?]
           [rand lc-exp?]])

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      [var-exp (var) (symbol->string var)]
      [lambda-exp (bound-var body)
                  (string-append "(lambda ("
                                 (symbol->string bound-var)
                                 ") "
                                 (unparse-lc-exp body)
                                 ")")]
      [app-exp (rator rand)
               (string-append "("
                              (unparse-lc-exp rator)
                              " "
                              (unparse-lc-exp rand)
                              ")")])))

(provide var-exp lambda-exp app-exp unparse-lc-exp)
