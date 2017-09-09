#lang eopl

;; Exercise 2.14 [★★] Extend the representation of the preceding exercise to include a third procedure that implements
;; has-binding? (see exercise 2.9).

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define empty-env
  (lambda ()
    (list (lambda (search-var)
            (report-no-binding-found search-var))
          (lambda ()
            #t)
          (lambda (search-var)
            #f))))

(define empty-env?
  (lambda (env)
    ((cadr env))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda (search-var)
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var)))
          (lambda ()
            #f)
          (lambda (search-var)
            (or (eqv? saved-var search-var)
                (has-binding? saved-env search-var))))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define has-binding?
  (lambda (env search-var)
    ((caddr env) search-var)))

(provide empty-env empty-env? extend-env apply-env has-binding?)
