#lang racket

(require profile)
(require optimization-coach)
(require rackunit)
(require "exercise-2.1.rkt")

(define (is-natural n expected)
  (if (zero? expected)
      (is-zero? n)
      (and (not (is-zero? n))
           (is-natural (predecessor n) (- expected 1)))))

(define-binary-check (check-natural is-natural actual expected))

(define (from-integer-helper base n)
  (if (zero? n)
      base
      (from-integer-helper (successor base) (- n 1))))

(define (from-integer n)
  (from-integer-helper (zero) n))

(for ([i 100])
  (check-natural (from-integer i) i))

(check-natural (factorial (from-integer 0)) 1)
(check-natural (factorial (from-integer 1)) 1)
(check-natural (factorial (from-integer 2)) 2)
(check-natural (factorial (from-integer 3)) 6)
(check-natural (factorial (from-integer 4)) 24)
(check-natural (factorial (from-integer 5)) 120)
(check-natural (factorial (from-integer 10)) 3628800)
