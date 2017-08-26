#lang racket

(require rackunit)
(require "exercise-1.13.rkt")

(check-equal? (subst 'b 'a '()) '())
(check-equal? (subst 'b 'a '(a)) '(b))
(check-equal? (subst 'b 'a '(b)) '(b))
(check-equal? (subst 'b 'a '(c)) '(c))
(check-equal? (subst 'b 'a '(a b a c d a)) '(b b b c d b))
(check-equal? (subst 'b 'a '((a) b a c d a)) '((b) b b c d b))
(check-equal? (subst 'b 'a '((x (a)) b a c d a)) '((x (b)) b b c d b))
(check-equal? (subst 'b 'a '(() ((x a (y a) (a z))))) '(() ((x b (y b) (b z)))))
