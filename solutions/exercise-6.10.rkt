#lang eopl

;; Exercise 6.10 [★] For list-sum, formulate a succinct representation of the continuations, like the one for fact/k
;; above.

(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
        cont
        (list-sum/k (cdr loi)
                    (+ cont (car loi))))))

(provide list-sum list-sum/k)
