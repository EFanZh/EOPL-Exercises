#lang racket

(require rackunit)
(require "../solutions/exercise-5.x-letrec-lang-registers.rkt")

(check-equal? (run "let f = proc ()
                              3
                    in (f)")
              (num-val 3))

(check-equal? (run "let f = proc (x, y)
                              -(x, y)
                    in (f 0 1)")
              (num-val -1))

(check-equal? (run "let f = proc (x, y, z)
                              x
                    in (f 2 3 5)")
              (num-val 2))

(check-equal? (run "let f = proc (x, y, z)
                              y
                    in (f 2 3 5)")
              (num-val 3))

(check-equal? (run "let f = proc (x, y, z)
                              z
                    in (f 2 3 5)")
              (num-val 5))

(check-equal? (run "letrec f() = 6
                    in (f)")
              (num-val 6))

(check-equal? (run "letrec f(x) = -(x, 2)
                    in (f 4)")
              (num-val 2))

(check-equal? (run "letrec f(x, y) = -(x, y)
                    in (f 7 2)")
              (num-val 5))
