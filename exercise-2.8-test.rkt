#lang racket

(require rackunit)
(require "exercise-2.8.rkt")

(check-true (empty-env? (empty-env)))
(check-false (empty-env? (extend-env 'a 1 (empty-env))))
