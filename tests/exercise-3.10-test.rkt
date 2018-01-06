#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-let-lang.rkt")

(check-equal? (run "let x = 4
                    in list(x, -(x, 1), -(x, 3))")
              (pair-val (num-val 4)
                        (pair-val (num-val 3)
                                  (pair-val (num-val 1)
                                            (emptylist-val)))))
