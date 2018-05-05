#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-call-by-need-lang.rkt")

(check-equal? (run "let makerec = proc (f)
                                    let d = proc (x)
                                              (f (x x))
                                    in (f (d d))
                    in let maketimes4 = proc (f)
                                          proc (x)
                                            if zero?(x)
                                            then 0
                                            else -((f -(x, 1)), -4)
                       in let times4 = (makerec maketimes4)
                          in (times4 3)")
              (num-val 12))

(check-equal? (run "let makerec = proc (f)
                                    let d = proc (x)
                                              proc (z)
                                                ((f (x x)) z)
                                    in proc (n)
                                         ((f (d d)) n)
                    in let maketimes4 = proc (f)
                                          proc (x)
                                            if zero?(x)
                                            then 0
                                            else -((f -(x, 1)), -4)
                       in let times4 = (makerec maketimes4)
                          in (times4 3)")
              (num-val 12))
