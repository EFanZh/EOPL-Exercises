#lang racket

(require rackunit)
(require "../solutions/exercise-2.9.rkt")

(check-false (has-binding? (empty-env) 'a))
(check-true (has-binding? (extend-env 'a 1 (empty-env)) 'a))
(check-false (has-binding? (extend-env 'a 1 (empty-env)) 'b))
(check-true (has-binding? (extend-env 'b 2 (extend-env 'a 1 (empty-env))) 'a))
(check-true (has-binding? (extend-env 'b 2 (extend-env 'a 1 (empty-env))) 'b))
(check-false (has-binding? (extend-env 'b 2 (extend-env 'a 1 (empty-env))) 'c))
