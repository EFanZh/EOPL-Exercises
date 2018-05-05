#lang racket/base

(require rackunit)
(require "../solutions/exercise-6.10.rkt")

(define (list-sum-2 loi)
  (list-sum/k loi 0))

(for ([f (list list-sum list-sum-2)])
  (check-eq? (f '()) 0)
  (check-eq? (f '(0)) 0)
  (check-eq? (f '(1)) 1)
  (check-eq? (f '(2)) 2)
  (check-eq? (f '(0 1)) 1)
  (check-eq? (f '(0 1 2)) 3)
  (check-eq? (f '(0 1 2 3)) 6)
  (check-eq? (f '(0 1 2 3 4)) 10))
