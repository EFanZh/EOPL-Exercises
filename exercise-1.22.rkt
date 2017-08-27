#lang eopl

;; Exercise 1.22 [🟉🟉] (filter-in pred lst) returns the list of those elements in lst that satisfy the predicate pred.
;;
;;     > (filter-in number? '(a 2 (1 3) b 7))
;;     (2 7)
;;     > (filter-in symbol? '(a (b c) 17 foo))
;;     (a foo)

(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (let ([element (car lst)]
              [tail (filter-in pred (cdr lst))])
          (if (pred element)
              (cons element tail)
              tail)))))

(provide filter-in)
