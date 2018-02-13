#lang racket

(require rackunit)
(require "../solutions/exercise-4.x-call-by-need-lang.rkt")

(check-equal? (run "let f = proc (x)
                              -(x, 1)
                    in (f 3)")
              (num-val 2))

(check-equal? (run "let f = proc (g)
                              (g 4)
                    in (f proc (x)
                            -(x, 3))")
              (num-val 1))
