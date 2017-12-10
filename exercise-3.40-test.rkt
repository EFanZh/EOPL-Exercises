#lang racket

(require rackunit)
(require "exercise-3.x-lexaddr-lang.rkt")

(check-equal? (run "letrec double(x) = if zero?(x)
                                       then 0
                                       else -((double -(x,1)), -2)
                    in (double 6)")
              (num-val 12))
