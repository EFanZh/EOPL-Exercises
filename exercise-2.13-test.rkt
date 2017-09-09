#lang racket

(require rackunit)
(require "exercise-2.13.rkt")

(check-pred empty-env? (empty-env))
(check-false (empty-env? (extend-env 'a 1 (empty-env))))
(check-false (empty-env? (extend-env 'b 2 (extend-env 'a 1 (empty-env)))))
(check-eqv? (apply-env (extend-env 'b 2 (extend-env 'a 1 (empty-env))) 'a) 1)
(check-eqv? (apply-env (extend-env 'b 2 (extend-env 'a 1 (empty-env))) 'b) 2)
(check-eqv? (apply-env (extend-env 'a 3 (extend-env 'b 2 (extend-env 'a 1 (empty-env)))) 'a) 3)
