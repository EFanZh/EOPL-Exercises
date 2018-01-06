#lang racket

(require rackunit)
(require "../solutions/exercise-3.13.rkt")

(check-equal? (run "if zero?(0) then 5 else 7")
              (num-val 5))

(check-equal? (run "if zero?(1) then 5 else 7")
              (num-val 7))

(check-equal? (run "if -1 then 5 else 7")
              (num-val 5))

(check-equal? (run "if 0 then 5 else 7")
              (num-val 7))

(check-equal? (run "if 1 then 5 else 7")
              (num-val 5))
