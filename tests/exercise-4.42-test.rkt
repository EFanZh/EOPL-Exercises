#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-call-by-need-lang.rkt")

(check-equal? (run "let* x = 3
                    in x")
              (num-val 3))

(check-equal? (run "let* f = proc (x)
                               x
                    in (f 4)")
              (num-val 4))

(check-equal? (run "let x = 2
                    in let* y = x
                       in begin set y = 3;
                                 x
                          end")
              (num-val 3))

(check-equal? (run "let x = 2
                    in let y = begin set x = -(x, 1);
                                     x
                               end
                       in let a = x
                          in let b = y
                             in -(a, b)")
              (num-val 0))

(check-equal? (run "let x = 2
                    in let* y = begin set x = -(x, 1);
                                      x
                                end
                       in let a = x
                          in let b = y
                             in -(a, b)")
              (num-val 1))
