#lang racket/base

(require rackunit)
(require "../solutions/exercise-2.20.rkt")

(check-equal? (number->bintree 13) '((13 () ()) . ()))

(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))

(check-equal? t1
              '((13
                 (12 () ())
                 (14 () ())) . ()))

(check-equal? (move-to-left-son t1) '((12 () ()) . ((13 right (14 () ())))))
(check-equal? (move-to-right-son t1) '((14 () ()) . ((13 left (12 () ())))))
(check-eqv? (current-element (move-to-left-son t1)) 12)
(check-eqv? (current-element (move-to-right-son t1)) 14)
(check-true (at-leaf? (move-to-right-son (move-to-left-son t1))))

(check-equal? (insert-to-left 15 t1)
              '((13
                 (15
                  (12 () ())
                  ())
                 (14 () ()))) . ())

(check-equal? (insert-to-right 15 t1)
              '((13
                 (12 () ())
                 (15
                  ()
                  (14 () ()))) . ()))

(check-eqv? (current-element t1) 13)
(check-equal? (move-up (move-up (move-to-left-son (move-to-left-son t1)))) t1)
(check-equal? (move-up (move-up (move-to-right-son (move-to-left-son t1)))) t1)
(check-equal? (move-up (move-up (move-to-left-son (move-to-right-son t1)))) t1)
(check-equal? (move-up (move-up (move-to-right-son (move-to-right-son t1)))) t1)
(check-true (at-root? t1))
(check-false (at-root? (move-to-left-son t1)))
