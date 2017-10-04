#lang racket

(require rackunit)
(require "exercise-3.x-proc-lang.rkt")

(check-equal? (run "(let x = 5
                     in let y = 4
                        in let x = 3
                           in proc (z)
                                -(z, x)
                     9)")
              (num-val 6))
