#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-let-lang.rkt")

(check-equal? (run "minus(1)") (num-val -1))
(check-equal? (run "minus(-(4, 2))") (num-val -2))
(check-equal? (run "minus(-(2, 4))") (num-val 2))
