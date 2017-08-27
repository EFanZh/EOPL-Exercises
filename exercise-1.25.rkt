#lang eopl

;; Exercise 1.25 [🟉🟉] (exists? pred lst) returns #t if any element of lst satisfies pred, and returns #f otherwise.
;;
;;     > (exists? number? '(a b c 3 e))
;;     #t
;;     > (exists? number? '(a b c d e))
;;     #f

(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst))
            (exists? pred (cdr lst))))))

(provide exists?)
