#lang eopl

;; Exercise 1.23 [🟉🟉] (list-index pred lst) returns the 0-based position of the first element of lst that satisfies the
;; predicate pred. If no element of lst satisfies the predicate, then list-index returns #f.
;;
;;     > (list-index number? '(a 2 (1 3) b 7))
;;     1
;;     > (list-index symbol? '(a (b c) 17 foo))
;;     0
;;     > (list-index symbol? '(1 2 (a b) 3))
;;     #f

(define list-index-helper
  (lambda (n pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (list-index-helper (+ n 1) pred (cdr lst))))))

(define list-index
  (lambda (pred lst)
    (list-index-helper 0 pred lst)))

(provide list-index)
