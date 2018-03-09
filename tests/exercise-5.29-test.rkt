#lang racket

(require rackunit)
(require "../solutions/exercise-5.29.rkt")

(check-equal? (fact-iter 0) 1)
(check-equal? (fact-iter 1) 1)
(check-equal? (fact-iter 2) 2)
(check-equal? (fact-iter 3) 6)
(check-equal? (fact-iter 4) 24)
(check-equal? (fact-iter 5) 120)
(check-equal? (fact-iter 6) 720)
(check-equal? (fact-iter 7) 5040)
(check-equal? (fact-iter 8) 40320)
(check-equal? (fact-iter 9) 362880)
(check-equal? (fact-iter 10) 3628800)
