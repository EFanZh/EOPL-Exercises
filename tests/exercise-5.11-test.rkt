#lang racket

(require rackunit)
(require "../solutions/exercise-5.x-implicit-refs-lang.rkt")

(check-equal? (run "begin 2
                    end")
              (num-val 2))

(check-equal? (run "let x = 3
                    in begin set x = 5;
                             x
                       end")
              (num-val 5))

(check-equal? (run "let x = 7
                    in begin set x = 13;
                             set x = 17;
                             x
                       end")
              (num-val 17))
