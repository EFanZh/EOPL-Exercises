#lang racket

(require rackunit)
(require "exercise-2.23.rkt")

(check-false (identifier? 'lambda))
(check-true (identifier? 'a))
(check-true (identifier? 'b))
(check-true (identifier? 'c))
(check-false (identifier? 0))
(check-false (identifier? 1))
(check-false (identifier? 2))
(check-false (identifier? '()))
(check-false (identifier? '(1)))
(check-false (identifier? '(a)))
