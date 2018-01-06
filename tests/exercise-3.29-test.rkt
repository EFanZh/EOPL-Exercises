#lang racket

(require rackunit)
(require "../solutions/exercise-3.28.rkt")

(check-equal? (run "let a = 3
                    in let p = proc (z) a
                       in let f = proc (x) (p 0)
                          in let a = 5
                             in (f 2)")
              (num-val 5))

(check-equal? (run "let a = 3
                    in let p = proc (z) a
                       in let f = proc (a) (p 0)
                          in let a = 5
                             in (f 2)")
              (num-val 2))
