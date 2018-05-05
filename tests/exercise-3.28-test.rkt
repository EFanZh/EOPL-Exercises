#lang racket/base

(require rackunit)
(require "../solutions/exercise-3.28.rkt")

(check-equal? (run "let a = 3
                    in let p = proc (x) -(x, a)
                           a = 5
                       in -(a, (p 2))")
              (num-val 8))
