#lang racket

(require rackunit)
(require "exercise-1.27.rkt")

(check-equal? (flatten '(a b c)) '(a b c))
(check-equal? (flatten '((a) () (b ()) () (c))) '(a b c))
(check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e))
(check-equal? (flatten '(a b (() (c)))) '(a b c))
(check-equal? (flatten '()) '())
