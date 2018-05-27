#lang racket/base

(require (except-in rackunit check))
(require "../solutions/exercise-7.9.rkt")

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
(check-equal? (run "(proc (x : int) -(x, 1) 30)") (num-val 29))
(check-equal? (run "(proc (x : (int -> int)) -(x, 1) 30)") (num-val 29))
(check-equal? (run "let f = proc (x : int) -(x, 1) in (f 30)") (num-val 29))
(check-equal? (run "(proc (f : (int -> int)) (f 30) proc (x : int) -(x, 1))") (num-val 29))
(check-equal? (run "((proc (x : int) proc (y : int) -(x, y) 5) 6)") (num-val -1))
(check-equal? (run "let f = proc (x : int) proc (y : int) -(x, y) in ((f -(10, 5)) 6)") (num-val -1))

(check-equal? (run "let fix = proc (f : bool)
                                let d = proc (x : bool)
                                          proc (z : bool)
                                            ((f (x x)) z)
                                in proc (n : bool)
                                     ((f (d d)) n)
                    in let t4m = proc (f : bool)
                                   proc (x : bool)
                                     if zero?(x)
                                     then 0
                                     else -((f -(x, 1)), -4)
                       in let times4 = (fix t4m)
                          in (times4 3)")
              (num-val 12))

(check-equal? (run "letrec int f(x : int) = -(x, 1) in (f 33)") (num-val 32))
(check-equal? (run "letrec int f(x : int) = if zero?(x) then 0 else -((f -(x, 1)), -2) in (f 4)") (num-val 8))

(check-equal? (run "let m = -5
                    in letrec int f(x : int) = if zero?(x)
                                               then 0
                                               else -((f -(x, 1)), m)
                       in (f 4)")
              (num-val 20))

(check-equal? (run "letrec int even(odd : (int -> int)) = proc (x : int)
                                                            if zero?(x)
                                                            then 1
                                                            else (odd -(x, 1))
                    in letrec int odd(x : int) = if zero?(x)
                                                 then 0
                                                 else ((even odd) -(x, 1))
                       in (odd 13)")
              (num-val 1))

(check-equal? (check "11") 'int)
(check-equal? (check "-33") 'int)
(check-equal? (check "-(44, 33)") 'int)
(check-equal? (check "-(-(44, 33), 22)") 'int)
(check-equal? (check "-(55, -(22, 11))") 'int)
(check-equal? (check "zero?(-(3, 2))") 'bool)

(check-exn exn:fail?
           (λ ()
             (check "-(2, zero?(0))")))

(check-exn exn:fail?
           (λ ()
             (check "foo")))

(check-exn exn:fail?
           (λ ()
             (check "-(x, foo)")))

(check-equal? (check "if zero?(1) then 3 else 4") 'int)
(check-equal? (check "if zero?(0) then 3 else 4") 'int)

(check-equal? (check "if zero?(-(11, 12)) then 3 else 4") 'int)
(check-equal? (check "if zero?(-(11, 11)) then 3 else 4") 'int)
(check-equal? (check "if zero?(1) then -(22, 1) else -(22, 2)") 'int)
(check-equal? (check "if zero?(0) then -(22, 1) else -(22, 2)") 'int)

(check-exn exn:fail?
           (λ ()
             (check "if zero?(0) then 1 else zero?(1)")))

(check-exn exn:fail?
           (λ ()
             (check "if 1 then 11 else 12")))

(check-equal? (check "let x = 3 in x") 'int)
(check-equal? (check "let x = 3 in -(x, 1)") 'int)
(check-equal? (check "let x = -(4, 1) in -(x, 1)") 'int)
(check-equal? (check "let x = 3 in let y = 4 in -(x, y)") 'int)
(check-equal? (check "let x = 3 in let x = 4 in x") 'int)
(check-equal? (check "let x = 3 in let x = -(x, 1) in x") 'int)
(check-equal? (check "(proc (x : int) -(x, 1) 30)") 'int)

(check-exn exn:fail?
           (λ ()
             (check "(proc(x : (int -> int)) -(x, 1) 30)")))

(check-equal? (check "let f = proc (x : int) -(x, 1) in (f 30)") 'int)
(check-equal? (check "(proc (f : (int -> int)) (f 30) proc (x : int) -(x, 1))") 'int)


(check-equal? (check "((proc (x : int) proc (y : int) -(x, y) 5) 6)") 'int)
(check-equal? (check "let f = proc (x : int) proc (y : int) -(x, y) in ((f -(10, 5)) 3)") 'int)
(check-equal? (check "letrec int f(x : int) = -(x, 1) in (f 33)") 'int)
(check-equal? (check "letrec int f(x : int) = if zero?(x) then 0 else -((f -(x, 1)), -2) in (f 4)") 'int)

(check-equal? (check "let m = -5
                      in letrec int f(x : int) = if zero?(x)
                                                 then -((f -(x, 1)), m)
                                                 else 0
                         in (f 4)")
              'int)

(check-equal? (check "letrec int double (n : int) = if zero?(n)
                                                    then 0
                                                    else -((double -(n, 1)), -2)
                      in (double 3)")
              'int)

(check-equal? (check "proc (x : int) -(x, 1)") '(int -> int))
(check-equal? (check "proc (x : int) zero?(-(x, 1))") '(int -> bool))
(check-equal? (check "let f = proc (x : int) -(x, 1) in (f 4)") 'int)
(check-equal? (check "let f = proc (x : int) -(x, 1) in f") '(int -> int))
(check-equal? (check "proc (f : (int -> bool)) (f 3)") '((int -> bool) -> bool))

(check-exn exn:fail?
           (λ ()
             (check "proc (f : (bool -> bool)) (f 3)")))

(check-equal? (check "proc (x : int) proc (f : (int -> bool)) (f x)")
              '(int -> ((int -> bool) -> bool)))

(check-equal? (check "proc (x : int) proc (f : (int -> (int -> bool))) (f x)")
              '(int -> ((int -> (int -> bool)) -> (int -> bool))))

(check-exn exn:fail?
           (λ ()
             (check "proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))")))

(check-equal? (check "((proc(x : int) proc (y : int) -(x, y) 4) 3)") 'int)

(check-equal? (check "(proc (x : int) -(x, 1) 4)") 'int)

(check-equal? (check "letrec int f(x : int) = -(x, 1)
                      in (f 40)")
              'int)

(check-equal? (check "(proc (x : int)
                         letrec bool loop(x : bool) = (loop x)
                         in x
                       1)")
              'int)

(check-equal? (check "let times = proc (x : int)
                                    proc (y : int)
                                      -(x, y) % not really times
                      in letrec int fact(x : int) = if zero?(x)
                                                    then 1
                                                    else ((times x) (fact -(x, 1)))
                         in fact")
              '(int -> int))

(check-equal? (check "let times = proc (x : int)
                                    proc (y : int)
                                      -(x, y) % not really times
                      in letrec int fact(x : int) = if zero?(x)
                                                    then 1
                                                    else ((times x) (fact -(x, 1)))
                         in (fact 4)")
              'int)

(check-exn exn:fail?
           (λ ()
             (check "(1 2)")))

(check-exn exn:fail?
           (λ ()
             (run "(1 2)")))

(check-equal? (check "list(1)") '(listof int))
(check-equal? (check "list(1, 2)") '(listof int))
(check-equal? (check "list(zero?(0))") '(listof bool))
(check-equal? (check "list(zero?(0), zero?(1))") '(listof bool))

(check-exn exn:fail?
           (λ ()
             (check "list(1, zero?(0))")))

(check-equal? (check "cons(1, emptylist[int])") '(listof int))
(check-equal? (check "cons(zero?(0), emptylist[bool])") '(listof bool))
(check-equal? (check "cons(6, list(1, 4, 5))") '(listof int))
(check-equal? (check "null?(emptylist[int])") 'bool)
(check-equal? (check "null?(emptylist[bool])") 'bool)
(check-equal? (check "null?(list(5, 6))") 'bool)
(check-equal? (check "null?(list(zero?(5)))") 'bool)

(check-exn exn:fail?
           (λ ()
             (check "null?(1)")))

(check-equal? (check "emptylist[int]") '(listof int))
(check-equal? (check "emptylist[bool]") '(listof bool))
(check-equal? (check "car(emptylist[int])") 'int)
(check-equal? (check "car(emptylist[bool])") 'bool)
(check-equal? (check "car(list(3, 2, 4))") 'int)
(check-equal? (check "car(list(zero?(0)))") 'bool)

(check-exn exn:fail?
           (λ ()
             (check "car(1)")))

(check-equal? (check "cdr(emptylist[int])") '(listof int))
(check-equal? (check "cdr(emptylist[bool])") '(listof bool))
(check-equal? (check "cdr(list(3, 2, 4))") '(listof int))
(check-equal? (check "cdr(list(zero?(0)))") '(listof bool))

(check-exn exn:fail?
           (λ ()
             (check "cdr(1)")))

(check-equal? (run "list(1)") (list-val (num-val 1) (emptylist-val)))
(check-equal? (run "list(1, 2)") (list-val (num-val 1) (list-val (num-val 2) (emptylist-val))))
(check-equal? (run "list(zero?(0))") (list-val (bool-val #t) (emptylist-val)))
(check-equal? (run "list(zero?(0), zero?(1))") (list-val (bool-val #t) (list-val (bool-val #f) (emptylist-val))))
(check-equal? (run "cons(1, emptylist[int])") (list-val (num-val 1) (emptylist-val)))
(check-equal? (run "cons(zero?(0), emptylist[bool])") (list-val (bool-val #t) (emptylist-val)))

(check-equal? (run "cons(6, list(1, 4, 5))")
              (list-val (num-val 6)
                        (list-val (num-val 1) (list-val (num-val 4) (list-val (num-val 5) (emptylist-val))))))

(check-equal? (run "null?(emptylist[int])") (bool-val #t))
(check-equal? (run "null?(emptylist[bool])") (bool-val #t))
(check-equal? (run "null?(list(5, 6))") (bool-val #f))
(check-equal? (run "null?(list(zero?(5)))") (bool-val #f))

(check-equal? (run "emptylist[int]") (emptylist-val))
(check-equal? (run "emptylist[bool]") (emptylist-val))

(check-exn exn:fail?
           (λ ()
             (run "car(emptylist[int])")))

(check-exn exn:fail?
           (λ ()
             (run "car(emptylist[bool])")))

(check-equal? (run "car(list(3, 2, 4))") (num-val 3))
(check-equal? (run "car(list(zero?(0)))") (bool-val #t))

(check-exn exn:fail?
           (λ ()
             (run "cdr(emptylist[int])")))

(check-exn exn:fail?
           (λ ()
             (run "cdr(emptylist[bool])")))

(check-equal? (run "cdr(list(3, 2, 4))") (list-val (num-val 2) (list-val (num-val 4) (emptylist-val))))
(check-equal? (run "cdr(list(zero?(0)))") (emptylist-val))
