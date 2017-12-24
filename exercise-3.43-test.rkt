#lang racket

(require rackunit)
(require "exercise-3.x-lexaddr-lang.rkt")

(check-equal? (run "let x = 3
                    in let f = proc (y)
                                 -(y, x)
                       in (f 13)")
              (num-val 10))

(check-equal? (run "let x = 5
                    in let f = proc ()
                                 x
                       in let x = 3
                          in (f)")
              (num-val 5))

(check-equal? (run "let x = 5
                    in let f = proc ()
                                 x
                       in let y = 3
                          in (f)")
              (num-val 5))

(check-equal? (run "let x = 7
                    in let f = proc (y)
                                 -(y, x)
                       in let y = 10
                          in (f y)")
              (num-val 3))
