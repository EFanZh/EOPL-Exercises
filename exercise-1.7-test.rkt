#lang racket

(require rackunit)
(require "exercise-1.7.rkt")

(check-exn exn:fail? (λ () (nth-element '() 0)))
(check-exn exn:fail? (λ () (nth-element '() 1)))
(check-exn exn:fail? (λ () (nth-element '() 2)))
(check-eq? (nth-element '(a) 0) 'a)
(check-exn exn:fail? (λ () (nth-element '(a) 1)))
(check-exn exn:fail? (λ () (nth-element '(a) 2)))
(check-exn exn:fail? (λ () (nth-element '(a) 3)))
(check-eq? (nth-element '(a b) 0) 'a)
(check-eq? (nth-element '(a b) 1) 'b)
(check-exn exn:fail? (λ () (nth-element '(a b) 2)))
(check-exn exn:fail? (λ () (nth-element '(a b) 3)))
(check-exn exn:fail? (λ () (nth-element '(a b) 4)))
