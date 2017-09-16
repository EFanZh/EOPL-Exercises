#lang racket

(require rackunit)
(require "exercise-2.26.rkt")

(define (tree-equal? tree-1 tree-2)
  (cases red-blue-tree tree-1
    [red-node (lson-1 rson-1)
              (cases red-blue-tree tree-2
                [red-node (lson-2 rson-2) (and (tree-equal? lson-1 lson-2)
                                               (tree-equal? rson-1 rson-2))]
                [else #f])]
    [blue-node (sons-1)
               (cases red-blue-tree tree-2
                 [blue-node (sons-2)
                            (let loop ([sons-1 sons-1]
                                       [sons-2 sons-2])
                              (if (null? sons-1)
                                  (null? sons-2)
                                  (and (not (null? sons-2))
                                       (tree-equal? (car sons-1) (car sons-2))
                                       (loop (cdr sons-1) (cdr sons-2)))))]
                 [else #f])]
    [leaf-node (num-1)
               (cases red-blue-tree tree-2
                 [leaf-node (num-2) (= num-1 num-2)]
                 [else #f])]))

(define-binary-check (check-tree-equal? tree-equal? actual expected))

(check-tree-equal?  (mark-leaves-with-red-depth (red-node (blue-node (list (leaf-node 26)
                                                                           (leaf-node 12)))
                                                          (red-node (leaf-node 11)
                                                                    (blue-node (list (leaf-node 117)
                                                                                     (leaf-node 14))))))
                    (red-node (blue-node (list (leaf-node 1) (leaf-node 1)))
                              (red-node (leaf-node 2) (blue-node (list (leaf-node 2) (leaf-node 2))))))

(check-tree-equal? (mark-leaves-with-red-depth (leaf-node 0)) (leaf-node 0))
(check-tree-equal? (mark-leaves-with-red-depth (leaf-node 1)) (leaf-node 0))
(check-tree-equal? (mark-leaves-with-red-depth (leaf-node 2)) (leaf-node 0))

(check-tree-equal? (mark-leaves-with-red-depth (blue-node (list (leaf-node 3) (leaf-node 4))))
                   (blue-node (list (leaf-node 0) (leaf-node 0))))

(check-tree-equal? (mark-leaves-with-red-depth (blue-node (list (blue-node (list (leaf-node 5) (leaf-node 6)))
                                                                (leaf-node 7))))
                   (blue-node (list (blue-node (list (leaf-node 0) (leaf-node 0)))
                                    (leaf-node 0))))
