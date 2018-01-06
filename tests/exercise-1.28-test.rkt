#lang racket

(require rackunit)
(require "../solutions/exercise-1.28.rkt")

(check-equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(check-equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))
(check-equal? (merge '() '()) '())
(check-equal? (merge '() '(1 2 3)) '(1 2 3))
(check-equal? (merge '(1 2 3) '()) '(1 2 3))
