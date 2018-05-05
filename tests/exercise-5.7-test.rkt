#lang racket/base

(require rackunit)
(require "../solutions/exercise-5.x-letrec-lang.rkt")

(check-equal? (run "let
                    in 1")
              (num-val 1))

(check-equal? (run "let x = 4
                    in x")
              (num-val 4))

(check-equal? (run "let x = 3
                        y = 5
                    in x")
              (num-val 3))

(check-equal? (run "let x = 3
                        y = 5
                    in y")
              (num-val 5))

(check-equal? (run "let x = 3
                        y = 5
                    in let x = y
                           y = x
                       in x")
              (num-val 5))

(check-equal? (run "let x = 3
                        y = 5
                    in let x = y
                           y = x
                       in y")
              (num-val 3))
