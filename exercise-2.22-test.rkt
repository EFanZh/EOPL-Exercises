#lang racket

(require rackunit)
(require "exercise-2.22.rkt")

(check-pred empty-stack? (empty-stack))
(check-false (empty-stack? (push (empty-stack) 1)))
(check-false (empty-stack? (push (push (empty-stack) 1) 2)))
(check-eqv? (top (push (empty-stack) 1)) 1)
(check-pred empty-stack? (pop (push (empty-stack) 1)))
(check-eqv? (top (push (push (empty-stack) 1) 2)) 2)
(check-eqv? (top (pop (push (push (empty-stack) 1) 2))) 1)
