#lang racket

(require rackunit)
(require "exercise-2.5.rkt")

(check-eqv? (apply-env (extend-env 'a 7 (empty-env)) 'a) 7)

(check-eqv? (apply-env (extend-env 'a
                                   5
                                   (extend-env 'a
                                               7
                                               (empty-env)))
                       'a)
            5)

(check-eqv? (apply-env (extend-env 'b
                                   5
                                   (extend-env 'a
                                               7
                                               (empty-env)))
                       'a)
            7)

(check-eqv? (apply-env (extend-env 'b
                                   5
                                   (extend-env 'a
                                               7
                                               (empty-env)))
                       'b)
            5)
