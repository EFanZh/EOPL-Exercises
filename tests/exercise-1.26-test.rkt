#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.26.rkt")

(check-equal? (up '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (up '((x (y)) z)) '(x (y) z))
(check-equal? (up '()) '())
(check-equal? (up '(1 (2) (3 4 5) (6 (7 8)) 9)) '(1 2 3 4 5 6 (7 8) 9))
