#lang eopl

;; Exercise 2.25 [★★] Use cases to write max-interior, which takes a binary tree of integers (as in the preceding
;; exercise) with at least one interior node and returns the symbol associated with an interior node with a maximal leaf
;; sum.
;;
;;     > (define tree-1
;;         (interior-node 'foo (leaf-node 2) (leaf-node 3)))
;;     > (define tree-2
;;         (interior-node 'bar (leaf-node -1) tree-1))
;;     > (define tree-3
;;         (interior-node 'baz tree-2 (leaf-node 1)))
;;     > (max-interior tree-2)
;;     foo
;;     > (max-interior tree-3)
;;     baz
;;
;; The last invocation of max-interior might also have returned foo, since both the foo and baz nodes have a leaf sum of
;; 5.

(require "exercise-2.24.rkt")

(define-datatype bintree-info bintree-info?
  [leaf-info [num integer?]]
  [interior-info [sum integer?]
                 [max-sum integer?]
                 [max-key symbol?]])

(define max-interior-helper
  (lambda (tree)
    (cases bintree tree
      [leaf-node (num)
                 (leaf-info num)]
      [interior-node (key left right)
                     (let ([left-info (max-interior-helper left)]
                           [right-info (max-interior-helper right)])
                       (cases bintree-info left-info
                         [leaf-info (left-num)
                                    (cases bintree-info right-info
                                      [leaf-info (right-num)
                                                 (let ([sum (+ left-num right-num)])
                                                   (interior-info sum
                                                                  sum
                                                                  key))]
                                      [interior-info (right-sum right-max-sum right-max-key)
                                                     (let ([sum (+ left-num right-sum)])
                                                       (if (< sum right-max-sum)
                                                           (interior-info sum
                                                                          right-max-sum
                                                                          right-max-key)
                                                           (interior-info sum
                                                                          sum
                                                                          key)))])]
                         [interior-info (left-sum left-max-sum left-max-key)
                                        (cases bintree-info right-info
                                          [leaf-info (right-num)
                                                     (let ([sum (+ left-sum right-num)])
                                                       (if (< sum left-max-sum)
                                                           (interior-info sum
                                                                          left-max-sum
                                                                          left-max-key)
                                                           (interior-info sum
                                                                          sum
                                                                          key)))]
                                          [interior-info (right-sum right-max-sum right-max-key)
                                                         (let* ([sum (+ left-sum right-sum)]
                                                                [max-sum-and-key (if (< left-max-sum right-max-sum)
                                                                                     (cons right-max-sum right-max-key)
                                                                                     (cons left-max-sum left-max-key))]
                                                                [max-sum (car max-sum-and-key)]
                                                                [max-key (cdr max-sum-and-key)])
                                                           (if (< sum max-sum)
                                                               (interior-info sum
                                                                              max-sum
                                                                              max-key)
                                                               (interior-info sum
                                                                              sum
                                                                              key)))])]))])))

(define max-interior
  (lambda (tree)
    (cases bintree-info (max-interior-helper tree)
      [leaf-info (num) (eopl:error 'max-interior "~s is not an interior node" tree)]
      [interior-info (sum max-sum max-key) max-key])))

(provide leaf-node interior-node max-interior)
