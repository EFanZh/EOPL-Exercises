#lang eopl

;; Exercise 2.9 [★] Add to the environment interface an observer called has-binding? that takes an environment env and
;; a variable s and tests to see if s has an associated value in env. Implement it using the a-list representation.

(require "exercise-2.5.rkt")

(define has-binding?
  (lambda (env search-var)
    (cond [(null? env) #f]
          [(eqv? (caar env) search-var) #t]
          [else (has-binding? (cdr env) search-var)])))

(provide empty-env extend-env has-binding?)
