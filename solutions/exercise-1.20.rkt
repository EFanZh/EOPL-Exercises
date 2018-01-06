#lang eopl

;; Exercise 1.20 [★] (count-occurrences s slist) returns the number of occurrences of s in slist.
;;
;;     > (count-occurrences 'x '((f x) y (((x z) x))))
;;     3
;;     > (count-occurrences 'x '((f x) y (((x z) () x))))
;;     3
;;     > (count-occurrences 'w '((f x) y (((x z) x))))
;;     0

(define count-occurrences-sexp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? sexp s) 1 0)
        (count-occurrences s sexp))))

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-sexp s (car slist))
           (count-occurrences s (cdr slist))))))

(provide count-occurrences)
