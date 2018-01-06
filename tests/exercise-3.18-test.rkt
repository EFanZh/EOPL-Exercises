#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-let-lang.rkt")

(check-equal? (run "let u = 7
                    in unpack x y = cons(u, cons(3, emptylist))
                       in -(x, y)")
              (num-val 4))
