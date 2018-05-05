#lang racket/base

(require rackunit)
(require "../solutions/exercise-1.31.rkt")

(check-true (leaf? (leaf 0)))
(check-false (leaf? (interior-node 'a (leaf 0) (leaf 1))))
(check-true (leaf? (lson (interior-node 'a (leaf 0) (leaf 1)))))
(check-false (leaf? (lson (interior-node 'a (interior-node 'b (leaf 0) (leaf 1)) (leaf 2)))))
(check-true (leaf? (rson (interior-node 'a (leaf 0) (leaf 1)))))
(check-false (leaf? (rson (interior-node 'a (leaf 0) (interior-node 'b (leaf 1) (leaf 2))))))
(check-eqv? (contents-of (leaf 0)) 0)
(check-eqv? (contents-of (leaf 1)) 1)
(check-eqv? (contents-of (interior-node 'a (leaf 0) (leaf 1))) 'a)
(check-eqv? (contents-of (lson (interior-node 'b (leaf 0) (leaf 1)))) 0)
(check-eqv? (contents-of (rson (interior-node 'b (leaf 0) (leaf 1)))) 1)
