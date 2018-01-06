#lang racket

(require rackunit)
(require "../solutions/exercise-2.7.rkt")

(check-exn exn:fail?
           (λ ()
             (apply-env (empty-env) 'b)))

(check-eqv? (apply-env (extend-env 'a 1 (empty-env)) 'a)
            1)

(check-exn exn:fail?
           (λ ()
             (apply-env (extend-env 'a 1 (empty-env)) 'b)))

(check-exn exn:fail?
           (λ ()
             (apply-env (extend-env'c 2 (extend-env 'a 1 (empty-env))) 'b)))
