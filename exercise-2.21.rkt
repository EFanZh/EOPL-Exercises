#lang eopl

;; Exercise 2.21 [★] Implement the data type of environments, as in section 2.2.2, using define-datatype. Then include
;; has-binding? of exercise 2.9.

(define scheme-value?
  (lambda (value)
    #t))

(define-datatype env-type env?
  (empty-env)
  (extend-env [var symbol?]
              [val scheme-value?]
              [env env?]))

(define apply-env
  (lambda (env search-var)
    (cases env-type env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-var)]
      [extend-env (var val env) (if (eqv? var search-var)
                                    val
                                    (apply-env env search-var))])))

(define has-binding?
  (lambda (env search-var)
    (cases env-type env
      [empty-env () #f]
      [extend-env (var val env) (or (eqv? var search-var)
                                    (has-binding? env search-var))])))

(provide empty-env
         extend-env
         apply-env
         has-binding?)
