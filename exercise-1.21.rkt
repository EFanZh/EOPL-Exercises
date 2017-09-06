#lang eopl

;; Exercise 1.21 [★★] (product sos1 sos2), where sos1 and sos2 are each a list of symbols without repetitions, returns
;; a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
;;
;;     > (product '(a b c) '(x y))
;;     ((a x) (a y) (b x) (b y) (c x) (c y))

(define product-symbol
  (lambda (tail s sos)
    (if (null? sos)
        tail
        (product-symbol (cons (list s (car sos)) tail) s (cdr sos)))))

(define product-helper
  (lambda (tail sos1 sos2)
    (if (null? sos1)
        tail
        (product-helper (product-symbol tail (car sos1) sos2)
                        (cdr sos1)
                        sos2))))

(define product
  (lambda (sos1 sos2)
    (product-helper '() sos1 sos2)))

(provide product)
