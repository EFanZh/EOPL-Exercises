#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-implicit-refs-lang.rkt")

(check-equal? (run "let a = 3
                    in let b = 4
                       in let swap = proc (x)
                                       proc (y)
                                         let temp = deref(x)
                                         in begin setref(x, deref(y));
                                                  setref(y, temp)
                                            end
                          in begin ((swap ref a) ref b);
                                   -(a, b)
                             end")
              (num-val 1))

(check-equal? (run "let a = 3
                    in let r1 = ref a
                       in let r2 = ref a
                          in begin setref(r1, 4);
                                   deref(r2)
                             end")
              (num-val 4))
