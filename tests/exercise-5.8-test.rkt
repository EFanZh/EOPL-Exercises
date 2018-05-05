#lang racket/base

(require rackunit)
(require "../solutions/exercise-5.x-letrec-lang.rkt")

(check-equal? (run "let f = proc (x, y)
                              -(x, y)
                    in (f 0 0)")
              (num-val 0))

(check-equal? (run "let f = proc (x, y)
                              -(x, y)
                    in (f 0 1)")
              (num-val -1))

(check-equal? (run "let f = proc (x, y)
                              -(x, y)
                    in (f 1 0)")
              (num-val 1))

(check-equal? (run "let f = proc (x, y)
                              -(x, y)
                    in (f 1 1)")
              (num-val 0))

(check-equal? (run "let f = proc (x, y)
                              -(x, y)
                    in (f 7 4)")
              (num-val 3))

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
