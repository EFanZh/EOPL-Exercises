#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.9.rkt")

(initialize-store!)

(check-eq? (deref (newref 7)) 7)

(let ([ref1 (newref 13)]
      [ref2 (newref 17)]
      [ref3 (newref 24)])
  (check-eq? (deref ref1) 13)
  (check-eq? (deref ref2) 17)
  (check-eq? (deref ref3) 24)
  (setref! ref2 34)
  (check-eq? (deref ref1) 13)
  (check-eq? (deref ref2) 34)
  (check-eq? (deref ref3) 24))
