#lang racket

(require rackunit)
(require "exercise-1.31.rkt")
(require "exercise-1.35.rkt")

(define check-bin-tree-equal?
  (lambda (bin-tree-1 bin-tree-2)
    (if (leaf? bin-tree-1)
        (test-begin (check-true (leaf? bin-tree-2))
                    (check-eqv? (contents-of bin-tree-1) (contents-of bin-tree-2)))
        (test-begin (check-false (leaf? bin-tree-2))
                    (check-eqv? (contents-of bin-tree-1) (contents-of bin-tree-2))
                    (check-bin-tree-equal? (lson bin-tree-1) (lson bin-tree-2))
                    (check-bin-tree-equal? (rson bin-tree-1) (rson bin-tree-2))))))

(check-bin-tree-equal? (number-leaves (interior-node 'foo
                                                     (interior-node 'bar
                                                                    (leaf 26)
                                                                    (leaf 12))
                                                     (interior-node 'baz
                                                                    (leaf 11)
                                                                    (interior-node 'quux
                                                                                   (leaf 117)
                                                                                   (leaf 14)))))
                       (interior-node 'foo
                                      (interior-node 'bar
                                                     (leaf 0)
                                                     (leaf 1))
                                      (interior-node 'baz
                                                     (leaf 2)
                                                     (interior-node 'quux
                                                                    (leaf 3)
                                                                    (leaf 4)))))

(check-bin-tree-equal? (number-leaves (leaf 7)) (leaf 0))
