#lang racket

(require rackunit)
(require "exercise-4.x-explicit-refs-lang.rkt")

(check-equal? (run "let x = 4
                    in cons(x,
                            cons(cons(-(x, 1),
                                      emptylist),
                                 emptylist))")
              (pair-val (num-val 4)
                        (pair-val (pair-val (num-val 3)
                                            (emptylist-val))
                                  (emptylist-val))))

(check-equal? (run "car(cons(2, 3))") (num-val 2))
(check-equal? (run "cdr(cons(2, 3))") (num-val 3))
(check-equal? (run "null?(emptylist)") (bool-val #t))
(check-equal? (run "null?(1)") (bool-val #f))
(check-equal? (run "null?(cons(2, 3))") (bool-val #f))
(check-equal? (run "list()") (emptylist-val))
(check-equal? (run "list(1)") (pair-val (num-val 1) (emptylist-val)))
(check-equal? (run "list(3, 1)") (pair-val (num-val 3) (pair-val (num-val 1) (emptylist-val))))

(check-equal? (run "list(6, 8, 7)") (pair-val (num-val 6)
                                              (pair-val (num-val 8)
                                                        (pair-val (num-val 7)
                                                                  (emptylist-val)))))

(check-equal? (run "let x = 5
                    in let y = 6
                       in let z = 11
                          in let w = newref(z)
                             in list(y, x, deref(w))")
              (pair-val (num-val 6)
                        (pair-val (num-val 5)
                                  (pair-val (num-val 11)
                                            (emptylist-val)))))
