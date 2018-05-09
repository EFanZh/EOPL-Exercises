#lang racket/base

(require rackunit)
(require "../solutions/exercise-6.24.rkt")

(check-equal? (run "11") (num-val 11))
(check-equal? (run "-33") (num-val -33))
(check-equal? (run "-(44, 33)") (num-val 11))
(check-equal? (run "-(-(44, 33), 22)") (num-val -11))
(check-equal? (run "-(55, -(22, 11))") (num-val 44))

(check-exn exn:fail?
           (λ ()
             (run "foo")))

(check-exn exn:fail?
           (λ ()
             (run "-(x, foo)")))

(check-equal? (run "if zero?(0) then 3 else 4") (num-val 3))
(check-equal? (run "if zero?(1) then 3 else 4") (num-val 4))

(check-exn exn:fail?
           (λ ()
             (run "-(zero?(0), 1)")))

(check-exn exn:fail?
           (λ ()
             (run "-(1, zero?(0))")))

(check-exn exn:fail?
           (λ ()
             (run "if 1 then 2 else 3")))

(check-equal? (run "if zero?(-(11, 11)) then 3 else 4") (num-val 3))
(check-equal? (run "if zero?(-(11, 12)) then 3 else 4") (num-val 4))
(check-equal? (run "if zero?(-(11, 11)) then 3 else foo") (num-val 3))
(check-equal? (run "if zero?(-(11, 12)) then foo else 4") (num-val 4))
(check-equal? (run "let x = 3 in x") (num-val 3))
(check-equal? (run "let x = 3 in -(x, 1)") (num-val 2))
(check-equal? (run "let x = -(4, 1) in -(x, 1)") (num-val 2))
(check-equal? (run "let x = 3 in let y = 4 in -(x, y)") (num-val -1))
(check-equal? (run "let x = 3 in let x = 4 in x") (num-val 4))
(check-equal? (run "let x = 3 in let x = -(x, 1) in x") (num-val 2))
(check-equal? (run "(proc(x) -(x, 1) 30)") (num-val 29))
(check-equal? (run "let f = proc (x) -(x, 1) in (f 30)") (num-val 29))
(check-equal? (run "(proc(f)(f 30) proc(x)-(x, 1))") (num-val 29))
(check-equal? (run "((proc (x) proc (y) -(x, y) 5) 6)") (num-val -1))
(check-equal? (run "(proc (x y) -(x, y) 5 6)") (num-val -1))
(check-equal? (run "let f = proc(x y) -(x, y) in (f -(10, 5) 6)") (num-val -1))

(check-equal? (run "let fix = proc (f)
                                let d = proc (x)
                                          proc (z)
                                            ((f (x x)) z)
                                in proc (n)
                                     ((f (d d)) n)
                    in let t4m = proc (f)
                                   proc(x)
                                     if zero?(x)
                                     then 0
                                     else -((f -(x, 1)), -4)
                       in let times4 = (fix t4m)
                          in (times4 3)")
              (num-val 12))

(check-equal? (run "(proc (twice)
                       ((twice proc (z)
                                 -(z, 1))
                        11)
                     proc (f)
                       proc (x)
                         (f (f x)))")
              (num-val 9))

(check-equal? (run "let twice = proc(f x k)
                                  (f x
                                     proc (z)
                                       (f z k))
                    in (twice proc (x k)
                                (k -(x, 1))
                              11
                              proc(z)
                                z)")
              (num-val 9))

(check-equal? (run "let f = proc (x)
                              -(x, 1)
                    in (f 27)")
              (num-val 26))

(check-equal? (run "let f = proc (x)
                              -(x, 1)
                    in (f (f 27))")
              (num-val 25))

(check-equal? (run "let f = proc (x)
                              proc (y)
                                -(x, y)
                    in ((f 27) 4)")
              (num-val 23))

(check-equal? (run "let f = proc (x)
                              proc (y)
                                -(x, y)
                    in let g = proc (z)
                                 -(z, 1)
                       in ((f 27) (g 11))")
              (num-val 17))

(check-equal? (run "let f = proc (x)
                              -(x, 1)
                    in if zero?((f 1))
                       then 11
                       else 22")
              (num-val 11))

(check-equal? (run "+()") (num-val 0))
(check-equal? (run "+(2, 3, 4)") (num-val 9))
(check-equal? (run "letrec f(x) = 17 in 34") (num-val 34))
(check-equal? (run "letrec f(x y) = -(x, y) in -(34, 2)") (num-val 32))

(check-equal? (run "letrec even(x) = if zero?(x)
                                     then zero?(0)
                                     else (odd -(x, 1))
                           odd (x) = if zero?(x)
                                     then zero?(1)
                                     else (even -(x, 1))
                    in (even 5)")
              (bool-val #f))

(check-equal? (run "letrec fib(n) = if zero?(n)
                                    then 1
                                    else if zero?(-(n, 1))
                                         then 1
                                         else -((fib -(n, 1)), -(0, (fib -(n, 2))))
                    in (fib 5)")
              (num-val 8))

(check-equal? (run "letrec fib(n) = if zero?(n)
                                    then 1
                                    else if zero?(-(n, 1))
                                         then 1
                                         else -((fib -(n, 1)), -(0, (fib -(n, 2))))
                    in +((fib 1), 12, (fib 5))")
              (num-val 21))

(check-equal? (run "list()") (list-val '()))
(check-equal? (run "list(1)") (list-val (list (num-val 1))))
(check-equal? (run "list(1, 2)") (list-val (list (num-val 1) (num-val 2))))
(check-equal? (run "list(1, 2, 3)") (list-val (list (num-val 1) (num-val 2) (num-val 3))))
(check-equal? (run "list(list(1, 2), 3)") (list-val (list (list-val (list (num-val 1) (num-val 2))) (num-val 3))))
(check-equal? (run "list(1, list(2, 3))") (list-val (list (num-val 1) (list-val (list (num-val 2) (num-val 3))))))

(check-equal? (run "let double = proc (x)
                                   -(x, -(0, x))
                    in list((double 2), (double 3), (double 5))")
              (list-val (list (num-val 4) (num-val 6) (num-val 10))))
