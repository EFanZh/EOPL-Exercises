#lang eopl

;; Exercise 2.19 [★] A binary tree with empty leaves and with interior nodes labeled with integers could be represented
;; using the grammar
;;
;; Bintree ::= () | (Int Bintree Bintree)
;;
;; In this representation, implement the procedure number->bintree, which takes a number and produces a binary tree
;; consisting of a single node containing that number. Also implement current-element, move-to-left-son,
;; move-to-right-son, at-leaf?, insert-to-left, and insert-to-right. For example,
;;
;;     > (number->bintree 13)
;;     (13 () ())
;;     > (define t1 (insert-to-right 14
;;                                   (insert-to-left 12
;;                                                   (number->bintree 13))))
;;     > t1
;;     (13
;;      (12 () ())
;;      (14 () ()))
;;     > (move-to-left-son t1)
;;     (12 () ())
;;     > (current-element (move-to-left-son t1))
;;     12
;;     > (at-leaf? (move-to-right-son (move-to-left-son t1)))
;;     #t
;;     > (insert-to-left 15 t1)
;;     (13
;;      (15
;;       (12 () ())
;;       ())
;;      (14 () ()))

(define number->bintree
  (lambda (num)
    `(,num () ())))

(define current-element car)

(define move-to-left-son cadr)

(define move-to-right-son caddr)

(define at-leaf? null?)

(define insert-to-left
  (lambda (num bintree)
    (let ([root-value (car bintree)]
          [left-child (cadr bintree)]
          [right-child (caddr bintree)])
      `(,root-value (,num ,left-child ()) ,right-child))))

(define insert-to-right
  (lambda (num bintree)
    (let ([root-value (car bintree)]
          [left-child (cadr bintree)]
          [right-child (caddr bintree)])
      `(,root-value ,left-child (,num () ,right-child)))))

(provide number->bintree
         current-element
         move-to-left-son
         move-to-right-son
         at-leaf?
         insert-to-left
         insert-to-right)
