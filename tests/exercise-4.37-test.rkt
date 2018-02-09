#lang racket

(require rackunit)
(require "../solutions/exercise-4.37.rkt")

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
                    in let v1 = 77
                       in begin (f v1);
                                (f v1);
                                v1
                          end")
              (num-val 55))

(check-equal? (run "let g = proc (x)
                              -(x, 11)
                    in begin (proc (f)
                                let x = 77
                                in begin (f x);
                                         (f x);
                                         x
                                   end
                              g);
                             g
                       end")
              (num-val 55))

(check-equal? (run "let x = 200
                    in let f = proc (z)
                                 -(z, x)
                       in let x = 100
                          in let g = proc (z)
                                       -(z, x)
                             in let v1 = 1
                                in let v2 = 1
                                   in begin (f v1);
                                            (g v2);
                                            -(v1, v2)
                                      end")
              (num-val -100))

(check-equal? (run "letrec double(x) = if zero?(x)
                                       then 0
                                       else let y = -(x, 1)
                                            in begin (double y);
                                                     -(y, -2)
                                            end
                    in let x = 6
                       in begin (double x);
                                x
                          end")
              (num-val 12))

(check-equal? (run "let x = 0
                    in letrec even(dummy) = if zero?(x)
                                            then 1
                                            else begin set x = -(x, 1);
                                                       let result = 888
                                                       in begin (odd result);
                                                                result
                                                          end
                                                 end
                              odd(dummy) = if zero?(x)
                                           then 0
                                           else begin set x = -(x, 1);
                                                      let result = 888
                                                      in begin (even result);
                                                               result
                                                         end
                                                end
                       in begin set x = 13;
                                let result = 888
                                in begin (odd result);
                                         result
                                   end
                          end")
              (num-val 1))

(check-equal? (run "let g = let counter = 0
                            in proc (dummy)
                                 begin set counter = -(counter, -1);
                                       counter
                                 end
                    in let a = 11
                       in let b = 11
                          in begin (g a);
                                   (g b);
                                   -(a, b)
                             end")
              (num-val -1))

(check-equal? (run "let f = proc (x)
                              begin set x = 3;
                                    4
                              end
                        in let x = 5
                           in begin (f x);
                                    x
                              end")
              (num-val 4))
