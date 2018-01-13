#lang racket

(require rackunit)
(require "../solutions/exercise-4.x-call-by-reference-lang.rkt")

(check-equal? (run "let x = 2
                    in let y = 1
                       in let f = proc (x, y)
                                    begin set x = 7;
                                          set y = 4
                                    end
                          in begin (f x y);
                                   -(x, y)
                             end")
              (num-val 3))

(check-equal? (run "letrec plus(x, y) = if zero?(x)
                                        then y
                                        else (plus -(x, 1) -(y, -1))
                    in (plus 8 6)")
              (num-val 14))
