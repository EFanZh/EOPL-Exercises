#lang eopl

;; Exercise 1.33 [★★] Write a procedure mark-leaves-with-red-depth that takes a bintree (definition 1.1.7), and
;; produces a bintree of the same shape as the original, except that in the new tree, each leaf contains the number of
;; nodes between it and the root that contain the symbol red. For example, the expression
;;
;; (mark-leaves-with-red-depth
;;  (interior-node 'red
;;                 (interior-node 'bar
;;                                (leaf 26)
;;                                (leaf 12))
;;                 (interior-node 'red
;;                                (leaf 11)
;;                                (interior-node 'quux
;;                                               (leaf 117)
;;                                               (leaf 14)))))
;;
;; which is written using the procedures defined in exercise 1.31, should return the bintree
;;
;; (red
;;  (bar 1 1)
;;  (red 2 (quux 2 2)))

(require "exercise-1.31.rkt")

(define mark-leaves-with-red-depth-helper
  (lambda (bin-tree red-num)
    (if (leaf? bin-tree)
        (leaf red-num)
        (let* ([content (contents-of bin-tree)]
               [new-red-num (if (eqv? content 'red) (+ red-num 1) red-num)])
          (interior-node content
                         (mark-leaves-with-red-depth-helper (lson bin-tree) new-red-num)
                         (mark-leaves-with-red-depth-helper (rson bin-tree) new-red-num))))))

(define mark-leaves-with-red-depth
  (lambda (bin-tree)
    (mark-leaves-with-red-depth-helper bin-tree 0)))

(provide mark-leaves-with-red-depth)
