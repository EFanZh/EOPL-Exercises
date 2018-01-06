#lang racket

(require rackunit)
(require "../solutions/exercise-1.9.rkt")

(check-equal? (remove 'a '()) '())
(check-equal? (remove 'a '(a)) '())
(check-equal? (remove 'a '(a b)) '(b))
(check-equal? (remove 'a '(b)) '(b))
(check-equal? (remove 'a '(b c)) '(b c))
(check-equal? (remove 'b '(a b)) '(a))
(check-equal? (remove 'b '(a b c)) '(a c))
(check-equal? (remove 'b '(a b c b)) '(a c))
(check-equal? (remove 'b '(a b c b d)) '(a c d))
