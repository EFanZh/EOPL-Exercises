#lang racket/base

(require rackunit)
(require "../solutions/exercise-5.x-threads-lang.rkt")
(require "../solutions/exercise-5.52.rkt")

(check-equal? (run program) (num-val 3))
