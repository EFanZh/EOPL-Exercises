#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.8.rkt")

(check-equal? (remove-first 'a '()) '())
(check-equal? (remove-first 'a '(a)) '())
(check-equal? (remove-first 'a '(b)) '())
(check-equal? (remove-first 'a '(b c)) '())
(check-equal? (remove-first 'b '(a b c)) '(c))
(check-equal? (remove-first 'b '(a b c d)) '(c d))
(check-equal? (remove-first 'b '(a b c d b e f)) '(c d b e f))
