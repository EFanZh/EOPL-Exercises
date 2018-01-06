#lang eopl

;; Exercise 2.10 [★] Add to the environment interface a constructor extend-env*, and implement it using the a-list
;; representation. This constructor takes a list of variables, a list of values of the same length, and an environment,
;; and is specified by (extend-env* (var1 … vark) (val1 … valk) ⌈f⌉) = ⌈g⌉, where g(var) = vali if var = vari for some i
;; such that 1 ≤ i ≤ k, f(var) otherwise.

(require "exercise-2.5.rkt")

(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env* (cdr vars)
                     (cdr vals)
                     (cons (cons (car vars) (car vals)) env)))))

(provide empty-env apply-env extend-env*)
