#lang eopl

;; Exercise 2.24 [★] Here is a definition of binary trees using define-datatype.
;;
;;     (define-datatype bintree bintree?
;;       (leaf-node
;;        (num integer?))
;;       (interior-node
;;        (key symbol?)
;;        (left bintree?)
;;        (right bintree?)))
;;
;; Implement a bintree-to-list procedure for binary trees, so that
;; (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))) returns the list
;;
;;     (interior-node
;;      a
;;      (leaf-node 3)
;;      (leaf-node 4))

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      [leaf-node (num) `(leaf-node ,num)]
      [interior-node (key left right) (list 'interior-node
                                            key
                                            (bintree-to-list left)
                                            (bintree-to-list right))])))

(provide leaf-node interior-node bintree-to-list)
