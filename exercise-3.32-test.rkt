#lang racket

(require rackunit)
(require "exercise-3.x-letrec-lang.rkt")

(check-equal? (run "letrec even(x) = if zero?(x) then 1 else (odd -(x, 1))
                           odd(x) = if zero?(x) then 0 else (even -(x, 1))
                    in (odd 13)")
              (num-val 1))

(check-equal? (run "letrec even(x) = if zero?(x) then 1 else (odd -(x, 1))
                           odd(x) = if zero?(x) then 0 else (even -(x, 1))
                    in (odd 16)")
              (num-val 0))
