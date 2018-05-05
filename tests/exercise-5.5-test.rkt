#lang racket/base

(require rackunit)
(require "../solutions/exercise-5.x-letrec-lang.rkt")

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
