#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.22.rkt")

(check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))
