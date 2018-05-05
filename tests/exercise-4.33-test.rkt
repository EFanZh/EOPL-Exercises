#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-call-by-reference-lang.rkt")

(check-equal? (run "let f = proc (x)
                              set x = 4
                    in let a = 6
                       in begin (f a);
                                a
                          end")
              (num-val 4))

(check-equal? (run "let f = proc* (x)
                              set x = 4
                    in let a = 6
                       in begin (f a);
                                a
                          end")
              (num-val 6))

(check-equal? (run "letrec f(x) = set x = 4
                    in let a = 6
                       in begin (f a);
                                a
                          end")
              (num-val 4))

(check-equal? (run "letrec* f(x) = set x = 4
                    in let a = 6
                       in begin (f a);
                                a
                          end")
              (num-val 6))
