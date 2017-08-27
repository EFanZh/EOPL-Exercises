#lang eopl

;; Exercise 1.15 [🟉] (duple n x) returns a list containing n copies of x.
;;
;;     > (duple 2 3)
;;     (3 3)
;;     > (duple 4 '(ha ha))
;;     ((ha ha) (ha ha) (ha ha) (ha ha))
;;     > (duple 0 '(blah))
;;     ()

(define duple-helper
  (lambda (lst n x)
    (if (zero? n)
        lst
        (duple-helper (cons x lst) (- n 1) x))))

(define duple
  (lambda (n x)
    (duple-helper '() n x)))

(provide duple)
