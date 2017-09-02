#lang eopl

;; Exercise 1.35 [🟉🟉🟉] Write a procedure number-leaves that takes a bintree, and produces a bintree like the original,
;; except the contents of the leaves are numbered starting from 0. For example,
;;
;;     (number-leaves
;;      (interior-node 'foo
;;                     (interior-node 'bar
;;                                    (leaf 26)
;;                                    (leaf 12))
;;                     (interior-node 'baz
;;                                    (leaf 11)
;;                                    (interior-node 'quux
;;                                                   (leaf 117)
;;                                                   (leaf 14)))))
;;
;; should return
;;
;;     (foo
;;      (bar 0 1)
;;      (baz
;;       2
;;       (quux 3 4)))

(require "exercise-1.31.rkt")

(define number-leaves-helper
  (lambda (bin-tree n)
    (if (leaf? bin-tree)
        (cons (leaf n) (+ n 1))
        (let* ([left-result (number-leaves-helper (lson bin-tree) n)]
               [right-result (number-leaves-helper (rson bin-tree) (cdr left-result))])
          (cons (interior-node (contents-of bin-tree)
                               (car left-result)
                               (car right-result))
                (cdr right-result))))))

(define number-leaves
  (lambda (bin-tree)
    (car (number-leaves-helper bin-tree 0))))

(provide number-leaves)
