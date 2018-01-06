#lang eopl

;; Exercise 1.16 [★] (invert lst), where lst is a list of 2-lists (lists of length two), returns a list with each
;; 2-list reversed.
;;
;;     > (invert '((a 1) (a 2) (1 b) (2 b)))
;;     ((1 a) (2 a) (b 1) (b 2))

(define invert
  (lambda (lst)
    (map (lambda (x) (list (cadr x) (car x)))
         lst)))

(provide invert)
