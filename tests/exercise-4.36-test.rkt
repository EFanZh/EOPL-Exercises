#lang racket/base

(require rackunit)
(require "../solutions/exercise-4.x-call-by-reference-lang.rkt")

(check-equal? (run "let a = newarray(2, -99)
                    in let p = proc (x)
                                 let v = arrayref(x, 1)
                                 in arrayset(x, 1, -(v, -1))
                       in begin arrayset(a, 1, 0);
                                (p a);
                                (p a);
                                arrayref(a, 1)
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

(check-equal? (run "let swap = proc (x)
                                 proc (y)
                                   let temp = x
                                   in begin set x = y;
                                            set y = temp
                                      end
                    in let a = newarray(4, 3)
                       in let i = 1
                          in let j = 2
                             in begin arrayset(a, j, 5);
                                      ((swap arrayref(a, i)) arrayref(a, j));
                                      -(arrayref(a, 1), arrayref(a, 2))
                                end")
              (num-val 2))

(check-equal? (run "let swap = proc (x)
                                 proc (y)
                                   let temp = x
                                   in begin set x = y;
                                            set y = temp
                                      end
                    in let a = newarray(4, 0)
                       in let i = 1
                          in let j = 2
                             in begin arrayset(a, i, 3);
                                      arrayset(a, j, 5);
                                      ((swap arrayref(a, i)) arrayref(a, j));
                                      -(arrayref(a, 1), arrayref(a, 2))
                                end")
              (num-val 2))

(check-equal? (run "let swap = proc (x)
                                 proc (y)
                                   let temp = x
                                   in begin set x = y;
                                            set y = temp
                                      end
                    in let a = newarray(4, 0)
                       in let i = 1
                          in let j = 2
                             in begin arrayset(a, i, 3);
                                      arrayset(a, j, 5);
                                      ((swap arrayref(a, arrayref(a, i))) arrayref(a, j));
                                      arrayref(a, 0)
                                end")
              (num-val 0))

(check-equal? (run "let swap = proc (x)
                                 proc (y)
                                   let temp = x
                                   in begin set x = y;
                                            set y = temp
                                      end
                    in let a = newarray(4, 0)
                       in let i = 1
                          in let j = 2
                             in begin arrayset(a, i, 3);
                                      arrayset(a, j, 5);
                                      ((swap arrayref(a, arrayref(a, i))) arrayref(a, j));
                                      arrayref(a, 1)
                                end")
              (num-val 3))

(check-equal? (run "let swap = proc (x)
                                 proc (y)
                                   let temp = x
                                   in begin set x = y;
                                            set y = temp
                                      end
                    in let a = newarray(4, 0)
                       in let i = 1
                          in let j = 2
                             in begin arrayset(a, i, 3);
                                      arrayset(a, j, 5);
                                      ((swap arrayref(a, arrayref(a, i))) arrayref(a, j));
                                      arrayref(a, 2)
                                end")
              (num-val 0))

(check-equal? (run "let swap = proc (x)
                                 proc (y)
                                   let temp = x
                                   in begin set x = y;
                                            set y = temp
                                      end
                    in let a = newarray(4, 0)
                       in let i = 1
                          in let j = 2
                             in begin arrayset(a, i, 3);
                                      arrayset(a, j, 5);
                                      ((swap arrayref(a, arrayref(a, i))) arrayref(a, j));
                                      arrayref(a, 3)
                                end")
              (num-val 5))
