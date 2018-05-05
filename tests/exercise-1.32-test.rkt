#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.31.rkt")
(require "../solutions/exercise-1.32.rkt")

(define check-bin-tree-equal?
  (lambda (bin-tree-1 bin-tree-2)
    (if (leaf? bin-tree-1)
        (test-begin (check-true (leaf? bin-tree-2))
                    (check-eqv? (contents-of bin-tree-1) (contents-of bin-tree-2)))
        (test-begin (check-false (leaf? bin-tree-2))
                    (check-eqv? (contents-of bin-tree-1) (contents-of bin-tree-2))
                    (check-bin-tree-equal? (lson bin-tree-1) (lson bin-tree-2))
                    (check-bin-tree-equal? (rson bin-tree-1) (rson bin-tree-2))))))

(check-bin-tree-equal? (double-tree (leaf 0)) (leaf 0))
(check-bin-tree-equal? (double-tree (leaf 1)) (leaf 2))
(check-bin-tree-equal? (double-tree (leaf 2)) (leaf 4))
(check-bin-tree-equal? (double-tree (interior-node 'a (leaf 3) (leaf 4))) (interior-node 'a (leaf 6) (leaf 8)))
(check-bin-tree-equal? (double-tree (interior-node 'a (interior-node 'b (leaf 5) (leaf 6)) (leaf 7)))
                       (interior-node 'a (interior-node 'b (leaf 10) (leaf 12)) (leaf 14)))
