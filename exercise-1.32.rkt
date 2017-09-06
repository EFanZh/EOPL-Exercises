#lang eopl

;; Exercise 1.32 [★] Write a procedure double-tree that takes a bintree, as represented in definition 1.1.7, and
;; produces another bintree like the original, but with all the integers in the leaves doubled.

(require "exercise-1.31.rkt")

(define double-tree
  (lambda (bin-tree)
    (if (leaf? bin-tree)
        (leaf (* (contents-of bin-tree) 2))
        (interior-node (contents-of bin-tree)
                       (double-tree (lson bin-tree))
                       (double-tree (rson bin-tree))))))

(provide double-tree)
