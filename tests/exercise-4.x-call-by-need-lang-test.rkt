#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-call-by-need-lang.rkt")

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

(check-equal? (run "let f = proc (x) -(x, 11)
                    in (f (f 77))")
              (num-val 55))

(check-equal? (run "(proc (f) (f (f 77))
                     proc (x) -(x, 11))")
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

(check-equal? (run "let x = 0
                    in letrec even(dummy) = if zero?(x)
                                            then 1
                                            else begin set x = -(x, 1);
                                                       (odd 888)
                                                 end
                              odd(dummy) = if zero?(x)
                                           then 0
                                           else begin set x = -(x, 1);
                                                      (even 888)
                                                end
                       in begin set x = 13;
                                (odd 888)
                          end")
              (num-val 1))

(check-equal? (run "let g = let counter = 0
                            in proc (dummy)
                                 begin set counter = -(counter, -1);
                                       counter
                                 end
                    in let a = (g 11)
                       in let b = (g 11)
                          in -(a, b)")
              (num-val -1))

(check-equal? (run "left(newpair(2, 3))") (num-val 2))
(check-equal? (run "right(newpair(2, 3))") (num-val 3))

(check-equal? (run "let p = newpair(2, 3)
                    in let x = left(p)
                       in begin setleft p = 4;
                                let y = left(p)
                                in -(x, y)
                          end")
              (num-val -2))

(check-equal? (run "let p = newpair(2, 3)
                    in let x = right(p)
                       in begin setright p = 4;
                                let y = right(p)
                                in -(x, y)
                          end")
              (num-val -1))

(check-equal? (run "let p = proc (x)
                              set x = 4
                    in let a = 3
                       in begin (p a);
                                a
                          end")
              (num-val 4))

(check-equal? (run "let f = proc (x)
                              set x = 44
                    in let g = proc (y)
                                 (f y)
                       in let z = 55
                          in begin (g z);
                                   z
                             end")
              (num-val 44))

(check-equal? (run "let swap = proc (x)
                                 proc (y)
                                   let temp = x
                                   in begin set x = y;
                                            set y = temp
                                      end
                    in let a = 33
                       in let b = 44
                          in begin ((swap a) b);
                                   -(a, b)
                             end")
              (num-val 11))

(check-equal? (run "letrec infinite-loop(x) = (infinite-loop -(x, -1))
                    in let f = proc (z)
                                 11
                       in (f (infinite-loop 0))")
              (num-val 11))
