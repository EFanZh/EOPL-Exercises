#lang racket

(require rackunit)
(require "exercise-3.x-lexaddr-lang.rkt")

(check-equal? (run "cond zero?(1) ==> 2
                         zero?(3) ==> 4
                         zero?(0) ==> 5
                         zero?(6) ==> 7
                    end")
              (num-val 5))
