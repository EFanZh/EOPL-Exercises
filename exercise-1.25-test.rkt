#lang racket

(require rackunit)
(require "exercise-1.25.rkt")

(check-true (exists? number? '(a b c 3 e)))
(check-false (exists? number? '(a b c d e)))
(check-false (exists? number? '()))
