#lang eopl

;; Exercise 1.19 [★] (list-set lst n x) returns a list like lst, except that the n-th element, using zero-based
;; indexing, is x.
;;
;;     > (list-set '(a b c d) 2 '(1 2))
;;     (a b (1 2) d)
;;     > (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
;;     (1 5 10)

(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

(provide list-set)
