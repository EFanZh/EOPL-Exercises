#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-lexaddr-lang.rkt")

(check-equal? (run "let f = proc (y)
                              -(y, 4)
                    in (f 13)")
              (num-val 9))

(check-equal? (run "let f = proc (y)
                              -(y, 2)
                        x = 6
                    in (f x)")
              (num-val 4))

(check-equal? (run "let f = proc (x, y)
                              -(x, y)
                        x = 8
                        y = 1
                    in (f x y)")
              (num-val 7))
