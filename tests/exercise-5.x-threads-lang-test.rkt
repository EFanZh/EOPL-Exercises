#lang racket

(require rackunit)
(require "../solutions/exercise-5.x-threads-lang.rkt")

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

(check-equal? (run "let a = 0
                    in let b = 0
                       in begin set a = 6;
                                set b = 2;
                                -(a, b)
                          end")
              (num-val 4))

(check-equal? (run "[]") (list-val '()))
(check-equal? (run "[2]") (list-val (list (num-val 2))))
(check-equal? (run "[2, 3]") (list-val (list (num-val 2) (num-val 3))))
(check-equal? (run "[2, 3, 5]") (list-val (list (num-val 2) (num-val 3) (num-val 5))))
(check-equal? (run "car([2, 3])") (num-val 2))
(check-equal? (run "cdr([2, 3])") (list-val (list (num-val 3))))
(check-equal? (run "null?([])") (bool-val #t))
(check-equal? (run "null?([1])") (bool-val #f))

(check-equal? (with-output-to-string (λ ()
                                       (run "print(-(7, 3))")))
              "4\n")

(let* ([output (with-output-to-string (λ ()
                                        (run "letrec noisy (l) = if null?(l)
                                                                 then 0
                                                                 else begin print(car(l));
                                                                            yield();
                                                                            (noisy cdr(l))
                                                                      end
                                              in begin spawn(proc (d)
                                                               (noisy [1,2,3,4,5]));
                                                       spawn(proc (d)
                                                               (noisy [6,7,8,9,10]));
                                                       print(100);
                                                       33
                                                 end")))]
       [nums (map string->number (string-split output))])
  (check-equal? (filter (λ (x)
                          (memq x '(1 2 3 4 5)))
                        nums)
                '(1 2 3 4 5))
  (check-equal? (filter (λ (x)
                          (memq x '(6 7 8 9 10)))
                        nums)
                '(6 7 8 9 10)))

(let* ([output (with-output-to-string (λ ()
                                        (run "let buffer = 0
                                              in let mut = mutex()
                                                 in let producer = proc (n)
                                                                     letrec waitloop(k) = if zero?(k)
                                                                                          then begin set buffer = n;
                                                                                                     signal(mut)
                                                                                               end
                                                                                          else begin print(-(k, -200));
                                                                                                     (waitloop -(k, 1))
                                                                                               end
                                                                     in (waitloop 5)
                                                    in let consumer = proc (d)
                                                                        begin wait(mut);
                                                                              buffer
                                                                        end
                                                       in begin wait(mut);
                                                                spawn(proc (d)
                                                                        (producer 44));
                                                                print(300);
                                                                (consumer 86)
                                                          end")))]
       [nums (map string->number (string-split output))])
  (check-equal? nums '(300 205 204 203 202 201)))
