#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.31.rkt")
(require "../solutions/exercise-1.33.rkt")

(define check-bin-tree-equal?
  (lambda (bin-tree-1 bin-tree-2)
    (if (leaf? bin-tree-1)
        (test-begin (check-true (leaf? bin-tree-2))
                    (check-eqv? (contents-of bin-tree-1) (contents-of bin-tree-2)))
        (test-begin (check-false (leaf? bin-tree-2))
                    (check-eqv? (contents-of bin-tree-1) (contents-of bin-tree-2))
                    (check-bin-tree-equal? (lson bin-tree-1) (lson bin-tree-2))
                    (check-bin-tree-equal? (rson bin-tree-1) (rson bin-tree-2))))))

(check-bin-tree-equal? (mark-leaves-with-red-depth (interior-node 'red
                                                                  (interior-node 'bar
                                                                                 (leaf 26)
                                                                                 (leaf 12))
                                                                  (interior-node 'red
                                                                                 (leaf 11)
                                                                                 (interior-node 'quux
                                                                                                (leaf 117)
                                                                                                (leaf 14)))))
                       (interior-node 'red
                                      (interior-node 'bar (leaf 1) (leaf 1))
                                      (interior-node 'red (leaf 2) (interior-node 'quux (leaf 2) (leaf 2)))))

(check-bin-tree-equal? (mark-leaves-with-red-depth (leaf 0)) (leaf 0))
(check-bin-tree-equal? (mark-leaves-with-red-depth (leaf 1)) (leaf 0))
(check-bin-tree-equal? (mark-leaves-with-red-depth (leaf 2)) (leaf 0))

(check-bin-tree-equal? (mark-leaves-with-red-depth (interior-node 'a (leaf 3) (leaf 4)))
                       (interior-node 'a (leaf 0) (leaf 0)))

(check-bin-tree-equal? (mark-leaves-with-red-depth (interior-node 'a (interior-node 'b (leaf 5) (leaf 6)) (leaf 7)))
                       (interior-node 'a (interior-node 'b (leaf 0) (leaf 0)) (leaf 0)))
