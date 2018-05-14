#lang eopl

;; Exercise 2.30 [★★] The procedure parse-expression as defined above is fragile: it does not detect several possible
;; syntactic errors, such as (a b c), and aborts with inappropriate error messages for other expressions, such as
;; (lambda). Modify it so that it is robust, accepting any s-exp and issuing an appropriate error message if the s-exp
;; does not represent a lambda-calculus expression.

(define identifier?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? x 'lambda)))))

(define-datatype lc-exp lc-exp?
  [var-exp [var identifier?]]
  [lambda-exp [bound-var identifier?]
              [body lc-exp?]]
  [app-exp [rator lc-exp?]
           [rand lc-exp?]])

(define report-error
  (lambda (expected datum)
    (eopl:error 'parse-expression "Expect ~a, but got ~s." expected datum)))

(define parse-lambda-expression
  (lambda (datum)
    (let ([after-lambda (cdr datum)])
      (if (pair? after-lambda)
          (let ([bound-var-list (car after-lambda)]
                [after-bound-var-list (cdr after-lambda)])
            (if (pair? bound-var-list)
                (let ([bound-var (car bound-var-list)]
                      [after-bound-var (cdr bound-var-list)])
                  (if (identifier? bound-var)
                      (if (null? after-bound-var)
                          (if (pair? after-bound-var-list)
                              (let ([body (car after-bound-var-list)]
                                    [after-body (cdr after-bound-var-list)])
                                (if (null? after-body)
                                    (lambda-exp bound-var (parse-expression body))
                                    (report-error "null after body" after-body)))
                              (report-error "a pair after bound var list" after-bound-var-list))
                          (report-error "null after bound var" after-bound-var))
                      (report-error "an identifier" bound-var)))
                (report-error "a pair" bound-var-list)))
          (report-error "a pair after lambda" after-lambda)))))

(define parse-application-expression
  (lambda (datum)
    (let ([rator (car datum)]
          [after-rator (cdr datum)])
      (if (pair? after-rator)
          (let ([rand (car after-rator)]
                [after-rand (cdr after-rator)])
            (if (null? after-rand)
                (app-exp (parse-expression rator) (parse-expression rand))
                (report-error "null after rand" after-rand)))
          (report-error "a pair after rator" after-rator)))))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (if (eqv? datum 'lambda)
                               (report-error "an identifier" datum)
                               (var-exp datum))]
          [(pair? datum) (if (eqv? (car datum) 'lambda)
                             (parse-lambda-expression datum)
                             (parse-application-expression datum))]
          [else (report-error "a symbol or pair" datum)])))

(provide lc-exp var-exp lambda-exp app-exp parse-expression)
