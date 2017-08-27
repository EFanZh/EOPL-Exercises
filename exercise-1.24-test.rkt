#lang racket

(require rackunit)
(require "exercise-1.24.rkt")

(check-false (every? number? '(a b c 3 e)))
(check-true (every? number? '(1 2 3 5 4)))
(check-true (every? number? '()))
