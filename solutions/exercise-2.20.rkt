#lang eopl

;; Exercise 2.20 [★★★] In the representation of binary trees in exercise 2.19 it is easy to move from a parent node to
;; one of its sons, but it is impossible to move from a son to its parent without the help of context arguments. Extend
;; the representation of lists in exercise 2.18 to represent nodes in a binary tree. As a hint, consider representing
;; the portion of the tree above the current node by a reversed list, as in exercise 2.18.
;;
;; In this representation, implement the procedures from exercise 2.19. Also implement move-up and at-root?.

;; Bintree := () | (Int Bintree Bintree)
;; BintreeX := (Bintree . Listof(Parent))
;; Parent := (Int Symbol Bintree)

(define number->bintree
  (lambda (num)
    (cons `(,num () ()) '())))

(define current-element caar)

(define move-to-left-son
  (lambda (bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons left-son
            (cons (list value 'right right-son)
                  parents)))))

(define move-to-right-son
  (lambda (bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons right-son
            (cons (list value 'left left-son)
                  parents)))))

(define at-leaf?
  (lambda (bintree)
    (null? (car bintree))))

(define insert-to-left
  (lambda (num bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons `(,value (,num ,left-son ()) ,right-son)
            parents))))

(define insert-to-right
  (lambda (num bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons `(,value ,left-son (,num () ,right-son))
            parents))))

(define move-up
  (lambda (bintree)
    (let* ([current (car bintree)]
           [parents (cdr bintree)]
           [parent (car parents)]
           [parent-value (car parent)]
           [parent-other-branch (cadr parent)]
           [parent-other-son (caddr parent)]
           [rest-parents (cdr parents)])
      (if (eqv? parent-other-branch 'left)
          (cons (list parent-value parent-other-son current)
                rest-parents)
          (cons (list parent-value current parent-other-son)
                rest-parents)))))

(define at-root?
  (lambda (bintree)
    (null? (cdr bintree))))

(provide number->bintree
         current-element
         move-to-left-son
         move-to-right-son
         at-leaf?
         insert-to-left
         insert-to-right
         move-up
         at-root?)
