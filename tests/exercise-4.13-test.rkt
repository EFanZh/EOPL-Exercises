#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-explicit-refs-lang.rkt")

(check-equal? (run "let x = newref(7)
                    in let f = proc ()
                                 setref(x, 5)
                       in let y = deref(x)
                          in begin (f);
                                   let z = deref(x)
                                   in -(y, z)
                             end")
              (num-val 2))

(check-equal? (run "let plus = proc (x, y)
                                 -(x, -(0, y))
                    in (plus 3 (plus 4 (plus 5 6)))")
              (num-val 18))

(check-equal? (run "let f = proc (x, y, z)
                              list(x, y, z)
                    in (f 4 5 6)")
              (pair-val (num-val 4)
                        (pair-val (num-val 5)
                                  (pair-val (num-val 6)
                                            (emptylist-val)))))

(check-equal? (run "letrec reverse(values, tail) = if null?(values)
                                                   then tail
                                                   else (reverse cdr(values)
                                                                 cons(car(values), tail))
                           reverse-map(f, values, tail) = if null?(values)
                                                          then tail
                                                          else (reverse-map f
                                                                            cdr(values)
                                                                            cons((f car(values)), tail))
                           map(f, values) = (reverse (reverse-map f values emptylist) emptylist)
                           double(x) = -(x, -(0, x))
                    in (map double
                            list(4, 5, 6))")
              (pair-val (num-val 8)
                        (pair-val (num-val 10)
                                  (pair-val (num-val 12)
                                            (emptylist-val)))))
