#lang racket

(require rackunit)
(require "exercise-3.x-lexaddr-lang.rkt")

(check-equal? (run "let u = 7
                    in unpack x y = pack(u, 3)
                       in -(x, y)")
              (num-val 4))
