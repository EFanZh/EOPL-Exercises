#lang racket

(require rackunit)
(require "../solutions/exercise-2.29.rkt")

(check-equal? (parse-expression 'a) (var-exp 'a))
(check-equal? (parse-expression 'b) (var-exp 'b))
(check-equal? (parse-expression '(lambda (x) y)) (lambda-exp '(x) (var-exp 'y)))
(check-equal? (parse-expression '(lambda (x y) z)) (lambda-exp '(x y) (var-exp 'z)))
(check-equal? (parse-expression '(lambda (x) (lambda (y) z))) (lambda-exp '(x) (lambda-exp '(y) (var-exp 'z))))
(check-equal? (parse-expression '(a)) (app-exp (var-exp 'a) '()))
(check-equal? (parse-expression '(a b)) (app-exp (var-exp 'a) (list (var-exp 'b))))
(check-equal? (parse-expression '(a b c)) (app-exp (var-exp 'a) (list (var-exp 'b) (var-exp 'c))))
