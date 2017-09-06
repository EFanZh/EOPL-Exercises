#lang eopl

;; Exercise 1.26 [★★] (up lst) removes a pair of parentheses from each top-level element of lst. If a top-level element
;; is not a list, it is included in the result, as is. The value of (up (down lst)) is equivalent to lst, but
;; (down (up lst)) is not necessarily lst. (See exercise 1.17.)
;;
;;     > (up '((1 2) (3 4)))
;;     (1 2 3 4)
;;     > (up '((x (y)) z))
;;     (x (y) z)

(define extend-head
  (lambda (tail head)
    (if (null? head)
        tail
        (cons (car head) (extend-head tail (cdr head))))))

(define up-element
  (lambda (tail element)
    (if (list? element)
        (extend-head tail element)
        (cons element tail))))

(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (up-element (up (cdr lst)) (car lst)))))

(provide up)
