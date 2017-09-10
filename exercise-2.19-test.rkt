#lang racket

(require rackunit)
(require "exercise-2.19.rkt")

(check-equal? (number->bintree 13) '(13 () ()))

(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))

(check-equal? t1
              '(13
                (12 () ())
                (14 () ())))

(check-equal? (move-to-left-son t1) '(12 () ()))
(check-equal? (move-to-right-son t1) '(14 () ()))
(check-eqv? (current-element (move-to-left-son t1)) 12)
(check-eqv? (current-element (move-to-right-son t1)) 14)
(check-true (at-leaf? (move-to-right-son (move-to-left-son t1))))

(check-equal? (insert-to-left 15 t1)
              '(13
                (15
                 (12 () ())
                 ())
                (14 () ())))

(check-equal? (insert-to-right 15 t1)
              '(13
                (12 () ())
                (15
                 ()
                 (14 () ()))))
