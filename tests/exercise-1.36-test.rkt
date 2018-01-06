#lang racket

(require rackunit)
(require "../solutions/exercise-1.36.rkt")

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(check-equal? (number-elements '()) '())
(check-equal? (number-elements '(a)) '((0 a)))
(check-equal? (number-elements '(a b)) '((0 a) (1 b)))
(check-equal? (number-elements '(a b c)) '((0 a) (1 b) (2 c)))
(check-equal? (number-elements '(a b c d)) '((0 a) (1 b) (2 c) (3 d)))
