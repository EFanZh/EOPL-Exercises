#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-let-lang.rkt")

(check-equal? (run "let x = 30
                    in let x = -(x, 1)
                           y = -(x, 2)
                       in -(x, y)")
              (num-val 1))
