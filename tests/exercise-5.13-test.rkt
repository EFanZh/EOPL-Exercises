#lang racket

(require rackunit)
(require "../solutions/exercise-5.x-implicit-refs-lang.rkt")

(check-equal? (run "letrec fact(n) = if zero?(n)
                                     then 1
                                     else *(n, (fact -(n, 1)))
                    in (fact 4)")
              (num-val 24))

(check-equal? (run "let fact-iter = letrec fact-iter-acc(n) = proc (a)
                                                                if zero?(n)
                                                                then a
                                                                else ((fact-iter-acc -(n, 1)) *(n, a))
                                    in proc (n)
                                         ((fact-iter-acc n) 1)
                    in (fact-iter 4)")
              (num-val 24))
