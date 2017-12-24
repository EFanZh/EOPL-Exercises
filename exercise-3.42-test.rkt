#lang racket

(require rackunit)
(require "exercise-3.x-lexaddr-lang.rkt")

(check-equal? (run "(let x = 5
                     in let y = 4
                        in let x = 3
                           in proc (z)
                                -(z, x)
                     9)")
              (num-val 6))

(check-equal? (run "(let x = 6
                     in let y = 4
                        in proc ()
                             -(x, y))")
              (num-val 2))

(check-equal? (run "((proc (x, y)
                        proc ()
                          -(x, y)
                      7
                      3))")
              (num-val 4))
