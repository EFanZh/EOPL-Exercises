#lang racket

(require rackunit)
(require "../solutions/exercise-4.x-mutable-pairs-lang.rkt")

(check-equal? (run "let a = newarray(2, -99)
                        p = proc (x)
                              let v = arrayref(x, 1)
                              in arrayset(x, 1, -(v, -1))
                    in begin arrayset(a, 1, 0);
                             (p a);
                             (p a);
                             arrayref(a,1)
                       end")
              (num-val 2))

(check-equal? (run "arrayref(newarray(3, 2), 0)") (num-val 2))
(check-equal? (run "arrayref(newarray(3, 2), 1)") (num-val 2))
(check-equal? (run "arrayref(newarray(3, 2), 2)") (num-val 2))

(check-equal? (run "let a = newarray(3, 2)
                    in begin arrayset(a, 0, 4);
                             arrayref(a, 0)
                       end")
              (num-val 4))

(check-equal? (run "let a = newarray(3, 2)
                    in begin arrayset(a, 1, 4);
                             arrayref(a, 0)
                       end")
              (num-val 2))

(check-equal? (run "let a = newarray(3, 2)
                    in begin arrayset(a, 2, 4);
                             arrayref(a, 0)
                       end")
              (num-val 2))

(check-equal? (run "let a = newarray(3, 2)
                    in begin arrayset(a, 0, 4);
                             arrayref(a, 1)
                       end")
              (num-val 2))

(check-equal? (run "let a = newarray(3, 2)
                    in begin arrayset(a, 1, 4);
                             arrayref(a, 1)
                       end")
              (num-val 4))

(check-equal? (run "let a = newarray(3, 2)
                    in begin arrayset(a, 2, 4);
                             arrayref(a, 1)
                       end")
              (num-val 2))
