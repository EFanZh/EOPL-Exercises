#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-let-lang.rkt")

(check-equal? (run "+(3, 4)") (num-val 7))
(check-equal? (run "*(3, 7)") (num-val 21))
(check-equal? (run "/(4, 2)") (num-val 2))
(check-equal? (run "/(4, 3)") (num-val 1))
