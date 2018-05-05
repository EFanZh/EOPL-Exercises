#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-call-by-reference-lang.rkt")

(check-equal? (run "letref x = 3
                    in x")
              (num-val 3))

(check-equal? (run "let x = 3
                    in let y = x
                       in begin set y = 4;
                                x
                          end")
              (num-val 3))

(check-equal? (run "let x = 3
                    in letref y = x
                       in begin set y = 4;
                                x
                          end")
              (num-val 4))

(check-equal? (run "let x = 3
                    in letref y = -(x, 2)
                       in begin set y = 4;
                                x
                          end")
              (num-val 3))
