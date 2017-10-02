#lang racket

(require rackunit)
(require "exercise-3.x-proc-lang.rkt")

(check-equal? (run "let f = proc (x)
                              proc (y)
                                -(x, -(0, y))
                    in ((f 0) 0)")
              (num-val 0))

(check-equal? (run "let f = proc (x)
                              proc (y)
                                -(x, -(0, y))
                    in ((f 0) 1)")
              (num-val 1))

(check-equal? (run "let f = proc (x)
                              proc (y)
                                -(x, -(0, y))
                    in ((f 1) 0)")
              (num-val 1))

(check-equal? (run "let f = proc (x)
                              proc (y)
                                -(x, -(0, y))
                    in ((f 1) 1)")
              (num-val 2))

(check-equal? (run "let f = proc (x)
                              proc (y)
                                -(x, -(0, y))
                    in ((f 3) 4)")
              (num-val 7))
