#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.20.rkt")

(check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
(check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
(check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)
