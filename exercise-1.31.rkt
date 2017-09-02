#lang eopl

;; Exercise 1.31 [🟉] Write the following procedures for calculating on a bintree (definition 1.1.7): leaf and
;; interior-node, which build bintrees, leaf?, which tests whether a bintree is a leaf, and lson, rson, and contents-of,
;; which extract the components of a node. contents-of should work on both leaves and interior nodes.

(define leaf
  (lambda (int)
    int))

(define interior-node
  (lambda (symbol left-child right-child)
    (cons symbol (cons left-child right-child))))

(define leaf? integer?)

(define lson cadr)

(define rson cddr)

(define contents-of
  (lambda (bin-tree)
    (if (leaf? bin-tree)
        bin-tree
        (car bin-tree))))

(provide leaf interior-node leaf? lson rson contents-of)
