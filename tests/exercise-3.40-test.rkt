#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-lexaddr-lang.rkt")

(check-equal? (run "letrec double(x) = if zero?(x)
                                       then 0
                                       else -((double -(x, 1)), -2)
                    in (double 6)")
              (num-val 12))

(check-equal? (run "letrec even(x) = if zero?(x) then 1 else (odd -(x, 1))
                           odd(x) = if zero?(x) then 0 else (even -(x, 1))
                    in (even 7)")
              (num-val 0))

(check-equal? (run "letrec even(x) = if zero?(x) then 1 else (odd -(x, 1))
                           odd(x) = if zero?(x) then 0 else (even -(x, 1))
                    in (even 8)")
              (num-val 1))

(check-equal? (run "letrec even(x) = if zero?(x) then 1 else (odd -(x, 1))
                           odd(x) = if zero?(x) then 0 else (even -(x, 1))
                    in (odd 13)")
              (num-val 1))

(check-equal? (run "letrec even(x) = if zero?(x) then 1 else (odd -(x, 1))
                           odd(x) = if zero?(x) then 0 else (even -(x, 1))
                    in (odd 16)")
              (num-val 0))
