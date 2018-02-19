#lang racket

(require rackunit)
(require "../solutions/exercise-5.x-letrec-lang.rkt")

(check-equal? (run "let x = 2
                    in let y = 3
                       in let3 x = y
                               y = x
                               w = y
                       in -(x, w)")
              (num-val 0))


(check-equal? (run "let x = 2
                    in let y = 3
                       in let3 x = y
                               y = x
                               w = y
                       in -(x, y)")
              (num-val 1))
