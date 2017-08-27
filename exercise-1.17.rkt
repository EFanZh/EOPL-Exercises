#lang eopl

;; Exercise 1.17 [🟉] (down lst) wraps parentheses around each top-level element of lst.
;;
;;     > (down '(1 2 3))
;;     ((1) (2) (3))
;;     > (down '((a) (fine) (idea)))
;;     (((a)) ((fine)) ((idea)))
;;     > (down '(a (more (complicated)) object))
;;     ((a) ((more (complicated))) (object))

(define down
  (lambda (lst)
    (map (lambda (x) (list x))
         lst)))

(provide down)
