#lang racket

(require rackunit)
(require "exercise-2.24.rkt")

(check-equal? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
              '(interior-node a (leaf-node 3) (leaf-node 4)))

(check-equal? (bintree-to-list (leaf-node 3)) '(leaf-node 3))

(check-equal? (bintree-to-list (interior-node 'a (leaf-node 3) (interior-node 'b (leaf-node 4) (leaf-node 5))))
              '(interior-node a (leaf-node 3) (interior-node b (leaf-node 4) (leaf-node 5))))

(check-equal? (bintree-to-list (interior-node 'a (interior-node 'b (leaf-node 3) (leaf-node 4)) (leaf-node 5)))
              '(interior-node a (interior-node b (leaf-node 3) (leaf-node 4)) (leaf-node 5)))
