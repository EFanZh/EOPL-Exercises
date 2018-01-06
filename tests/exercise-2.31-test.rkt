#lang racket

(require rackunit)
(require "../solutions/exercise-2.31.rkt")

(check-equal? (parse-prefix-list '(- - 3 2 - 4 - 12 7))
              (diff-exp (diff-exp (const-exp 3)
                                  (const-exp 2))
                        (diff-exp (const-exp 4)
                                  (diff-exp (const-exp 12)
                                            (const-exp 7)))))
