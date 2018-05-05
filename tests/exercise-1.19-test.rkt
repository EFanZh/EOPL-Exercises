#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.19.rkt")

(check-equal? (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10))
