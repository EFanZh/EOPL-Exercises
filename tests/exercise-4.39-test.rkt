#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-call-by-need-lang.rkt")

(check-equal? (run "let x = 0
                    in let f = proc (y)
                                 begin y;
                                       y
                                 end
                       in (f begin set x = -(x, -1);
                                   x
                             end)")
              (num-val 1))
