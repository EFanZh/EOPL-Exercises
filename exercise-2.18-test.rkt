#lang racket

(require rackunit)
(require "exercise-2.18.rkt")

(check-equal? (number->sequence 7) '(7 () ()))
(check-equal? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)
(check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
(check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
(check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))

(check-true (at-left-end? '(7 () ())))
(check-true (at-left-end? '(7 () (1))))
(check-true (at-right-end? '(7 () ())))
(check-true (at-right-end? '(7 (1) ())))
(check-false (at-left-end? '(7 (1) ())))
(check-false (at-right-end? '(7 () (1))))
(check-exn exn:fail? (λ () (move-to-left '(7 () (1 2 3)))))
(check-exn exn:fail? (λ () (move-to-right '(7 (3 2 1) ()))))
