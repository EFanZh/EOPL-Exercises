#lang eopl

;; Exercise 1.36 [🟉🟉🟉] Write a procedure g such that number-elements from page 23 could be defined as
;;
;;     (define number-elements
;;       (lambda (lst)
;;         (if (null? lst) '()
;;             (g (list 0 (car lst)) (number-elements (cdr lst))))))

(define g
  (lambda (head tail)
    (cons head
          (map (lambda (item)
                 (list (+ (car item) 1) (cadr item)))
               tail))))

(provide g)
