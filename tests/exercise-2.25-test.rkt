#lang racket/base

(require rackunit)
(require "../solutions/exercise-2.25.rkt")

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))

(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))

(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(check-eqv? (max-interior tree-2) 'foo)
(check-eqv? (max-interior tree-3) 'baz)
