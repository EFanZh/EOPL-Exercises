#lang eopl

;; Exercise 2.7 [★] Rewrite apply-env in figure 2.1 to give a more informative error message.

(require "exercise-2.5.rkt")

(define empty-env? null?)

(provide empty-env extend-env empty-env?)
