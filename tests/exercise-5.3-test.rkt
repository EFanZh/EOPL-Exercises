#lang racket/base

(require rackunit)
(require "../solutions/exercise-5.x-letrec-lang.rkt")

(check-equal? (run "let x = 2
                    in let2 x = 3
                            y = x
                       in -(x, y)")
              (num-val 1))
