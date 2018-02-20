#lang racket

(require rackunit)
(require "../solutions/exercise-5.x-letrec-lang.rkt")

(check-equal? (run "list()") (emptylist-val))

(check-equal? (run "list(1)") (pair-val (num-val 1)
                                        (emptylist-val)))

(check-equal? (run "list(2, 3)") (pair-val (num-val 2)
                                           (pair-val (num-val 3)
                                                     (emptylist-val))))

(check-equal? (run "list(5, 7, 9)") (pair-val (num-val 5)
                                              (pair-val (num-val 7)
                                                        (pair-val (num-val 9)
                                                                  (emptylist-val)))))

(check-equal? (run "list(11, 13, 17, 19)") (pair-val (num-val 11)
                                                     (pair-val (num-val 13)
                                                               (pair-val (num-val 17)
                                                                         (pair-val (num-val 19)
                                                                                   (emptylist-val))))))
