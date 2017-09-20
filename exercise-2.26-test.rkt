#lang racket

(require rackunit)
(require "exercise-2.26.rkt")

(check-equal? (mark-leaves-with-red-depth (red-node (blue-node (list (leaf-node 26)
                                                                     (leaf-node 12)))
                                                    (red-node (leaf-node 11)
                                                              (blue-node (list (leaf-node 117)
                                                                               (leaf-node 14))))))
              (red-node (blue-node (list (leaf-node 1) (leaf-node 1)))
                        (red-node (leaf-node 2) (blue-node (list (leaf-node 2) (leaf-node 2))))))

(check-equal? (mark-leaves-with-red-depth (leaf-node 0)) (leaf-node 0))
(check-equal? (mark-leaves-with-red-depth (leaf-node 1)) (leaf-node 0))
(check-equal? (mark-leaves-with-red-depth (leaf-node 2)) (leaf-node 0))

(check-equal? (mark-leaves-with-red-depth (blue-node (list (leaf-node 3) (leaf-node 4))))
              (blue-node (list (leaf-node 0) (leaf-node 0))))

(check-equal? (mark-leaves-with-red-depth (blue-node (list (blue-node (list (leaf-node 5) (leaf-node 6)))
                                                           (leaf-node 7))))
              (blue-node (list (blue-node (list (leaf-node 0) (leaf-node 0)))
                               (leaf-node 0))))
