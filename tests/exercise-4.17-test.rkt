#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-implicit-refs-lang.rkt")

(check-equal? (run "let x = 7
                    in let f = proc ()
                                 set x = 5
                       in let y = x
                          in begin (f);
                                   let z = x
                                   in -(y, z)
                             end")
              (num-val 2))

(check-equal? (run "let plus = proc (x, y)
                                 -(x, -(0, y))
                    in (plus 3 (plus 4 (plus 5 6)))")
              (num-val 18))

(check-equal? (run "let x = 30
                    in let x = -(x, 1)
                           y = -(x, 2)
                       in -(x, y)")
              (num-val 1))
