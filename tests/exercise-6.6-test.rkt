#lang racket

(require rackunit)
(require "../solutions/exercise-6.6.rkt")

(define (run-solution solution)
  (solution 'x 'y identity))

(define expected-result '(+ (f (g x)) (h (j y))))

(check-equal? (run-solution solution1) expected-result)
(check-equal? (run-solution solution2) expected-result)
(check-equal? (run-solution solution3) expected-result)
(check-equal? (run-solution solution4) expected-result)
(check-equal? (run-solution solution5) expected-result)
(check-equal? (run-solution solution6) expected-result)
