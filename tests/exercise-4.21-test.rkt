#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-implicit-refs-lang.rkt")

(check-equal? (run "let x = 11
                    in let p = proc (y)
                                 -(y, x)
                       in -(setdynamic x = 17
                            during (p 22),
                            (p 13))")
              (num-val 3))

(check-equal? (run "let x = 2
                    in let y = setdynamic x = begin set x = 3;
                                                    5
                                              end
                               during x
                       in -(x, y)")
              (num-val -2))
