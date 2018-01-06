#lang eopl

;; Exercise 2.26 [★★] Here is another version of exercise 1.33. Consider a set of trees given by the following
;; grammar:
;;
;;     Red-blue-tree    ::= Red-blue-subtree
;;     Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;;                      ::= (blue-node {Red-blue-subtree}*)
;;                      ::= (leaf-node Int)
;;
;; Write an equivalent definition using define-datatype, and use the resulting interface to write a procedure that takes
;; a tree and builds a tree of the same shape, except that each leaf node is replaced by a leaf node that contains the
;; number of red nodes on the path between it and the root.

(define-datatype red-blue-tree red-blue-tree?
  [red-node [lson red-blue-tree?]
            [rson red-blue-tree?]]
  [blue-node [sons (list-of red-blue-tree?)]]
  [leaf-node [num integer?]])

(define mark-leaves-with-red-depth-helper
  (lambda (tree red-num)
    (cases red-blue-tree tree
      [red-node (lson rson) (let ([new-red-num (+ red-num 1)])
                              (red-node (mark-leaves-with-red-depth-helper lson new-red-num)
                                        (mark-leaves-with-red-depth-helper rson new-red-num)))]
      [blue-node (sons) (blue-node (map (lambda (son)
                                          (mark-leaves-with-red-depth-helper son red-num))
                                        sons))]
      [leaf-node (_) (leaf-node red-num)])))

(define mark-leaves-with-red-depth
  (lambda (tree)
    (mark-leaves-with-red-depth-helper tree 0)))

(provide red-blue-tree red-node blue-node leaf-node mark-leaves-with-red-depth)
