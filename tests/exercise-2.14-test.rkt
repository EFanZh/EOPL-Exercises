#lang racket

(require rackunit)
(require "../solutions/exercise-2.14.rkt")

(check-pred empty-env? (empty-env))
(check-false (empty-env? (extend-env 'a 1 (empty-env))))
(check-false (empty-env? (extend-env 'b 2 (extend-env 'a 1 (empty-env)))))
(check-eqv? (apply-env (extend-env 'b 2 (extend-env 'a 1 (empty-env))) 'a) 1)
(check-eqv? (apply-env (extend-env 'b 2 (extend-env 'a 1 (empty-env))) 'b) 2)
(check-eqv? (apply-env (extend-env 'a 3 (extend-env 'b 2 (extend-env 'a 1 (empty-env)))) 'a) 3)

(check-false (has-binding? (empty-env) 'a))
(check-true (has-binding? (extend-env 'a 1 (empty-env)) 'a))
(check-false (has-binding? (extend-env 'a 1 (empty-env)) 'b))

(let ([env (extend-env 'a 3 (extend-env 'b 2 (extend-env 'a 1 (empty-env))))])
  (check-true (has-binding? env 'a))
  (check-true (has-binding? env 'b))
  (check-false (has-binding? env 'c)))
