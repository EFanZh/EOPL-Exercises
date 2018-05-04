#lang racket

(require rackunit)
(require "../solutions/exercise-6.8-inlined.rkt")

(check-equal? (run "2") (num-val 2))
(check-equal? (run "-(3, 3)") (num-val 0))
(check-equal? (run "-(3, 4)") (num-val -1))
(check-equal? (run "-(4, 3)") (num-val 1))
(check-equal? (run "zero?(0)") (bool-val #t))
(check-equal? (run "zero?(4)") (bool-val #f))
(check-equal? (run "if zero?(0) then 7 else 11") (num-val 7))
(check-equal? (run "if zero?(2) then 7 else 11") (num-val 11))
(check-equal? (run "let x = 5 in x") (num-val 5))
(check-equal? (run "let x = 5 in let x = 3 in x") (num-val 3))

(check-equal? (run "let f = proc (x)
                              -(x, 11)
                    in (f (f 77))")
              (num-val 55))

(check-equal? (run "(proc (f)
                       (f (f 77))
                     proc (x)
                       -(x, 11))")
              (num-val 55))

(check-equal? (run "let x = 200
                    in let f = proc (z)
                                 -(z, x)
                       in let x = 100
                          in let g = proc (z)
                                       -(z, x)
                             in -((f 1), (g 1))")
              (num-val -100))

(check-equal? (run "letrec double(x) = if zero?(x)
                                       then 0
                                       else -((double -(x, 1)), -2)
                    in (double 6)")
              (num-val 12))

(check-equal? (run "list()") (list-val '()))
(check-equal? (run "list(2)") (list-val (list (num-val 2))))
(check-equal? (run "list(2, 3)") (list-val (list (num-val 2) (num-val 3))))
(check-equal? (run "list(2, 3, 5)") (list-val (list (num-val 2) (num-val 3) (num-val 5))))
(check-equal? (run "car(list(2, 3))") (num-val 2))
(check-equal? (run "cdr(list(2, 3))") (list-val (list (num-val 3))))
(check-equal? (run "null?(list())") (bool-val #t))
(check-equal? (run "null?(list(1))") (bool-val #f))

(check-equal? (run "try 2
                    catch (x) x")
              (num-val 2))

(check-equal? (run "try raise 3
                    catch (x) x")
              (num-val 3))

(check-equal? (run "try -(3, raise 5)
                    catch (x) 5")
              (num-val 5))

(check-equal? (run "try try -(3, raise 5)
                        catch (x) raise 7
                    catch (y) y")
              (num-val 7))
