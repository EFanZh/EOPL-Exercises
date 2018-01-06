#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-proc-lang.rkt")

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

(check-equal? (run "let makerec = proc (f)
                                    let maker = proc (maker)
                                                  proc (x)
                                                    let recurive-proc = (maker maker)
                                                    in ((f recurive-proc) x)
                                    in (maker maker)
                    in let maketimes4 = proc (f)
                                          proc (x)
                                            if zero?(x)
                                            then 0
                                            else -((f -(x, 1)), -4)
                       in let times4 = (makerec maketimes4)
                           in (times4 3)")
              (num-val 12))
