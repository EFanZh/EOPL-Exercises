#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.16.rkt")

(check-equal? (invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2)))
(check-equal? (invert '()) '())
