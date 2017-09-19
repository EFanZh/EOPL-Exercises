#lang racket

(require rackunit)
(require "exercise-2.29.rkt")

(define (lc-exp-equal? a b)
  (cases lc-exp a
    [var-exp (var-a)
             (cases lc-exp b
               [var-exp (var-b) (eqv? var-a var-b)]
               [else #f])]
    [lambda-exp (bound-vars-a body-a)
                (cases lc-exp b
                  [lambda-exp (bound-vars-b body-b)
                              (and (equal? bound-vars-a bound-vars-b)
                                   (lc-exp-equal? body-a body-b))]
                  [else #f])]
    [app-exp (rator-a rands-a)
             (cases lc-exp b
               [app-exp (rator-b rands-b)
                        (and (equal? rator-a rator-b)
                             (let loop ([rands-a rands-a]
                                        [rands-b rands-b])
                               (cond [(null? rands-a) (null? rands-b)]
                                     [(null? rands-b) #f]
                                     [(lc-exp-equal? (car rands-a) (car rands-b)) (loop (cdr rands-a) (cdr rands-b))]
                                     [else #f])))]
               [else #f])]
    [else #f]))

(define-binary-check (check-lc-exp-equal? lc-exp-equal? actual expected))

(check-lc-exp-equal? (parse-expression 'a) (var-exp 'a))
(check-lc-exp-equal? (parse-expression 'b) (var-exp 'b))
(check-lc-exp-equal? (parse-expression '(lambda (x) y)) (lambda-exp '(x) (var-exp 'y)))
(check-lc-exp-equal? (parse-expression '(lambda (x y) z)) (lambda-exp '(x y) (var-exp 'z)))
(check-lc-exp-equal? (parse-expression '(lambda (x) (lambda (y) z))) (lambda-exp '(x) (lambda-exp '(y) (var-exp 'z))))
(check-lc-exp-equal? (parse-expression '(a)) (app-exp (var-exp 'a) '()))
(check-lc-exp-equal? (parse-expression '(a b)) (app-exp (var-exp 'a) (list (var-exp 'b))))
(check-lc-exp-equal? (parse-expression '(a b c)) (app-exp (var-exp 'a) (list (var-exp 'b) (var-exp 'c))))
