#lang eopl

;; Exercise 2.3 [★★] Define a representation of all the integers (negative and nonnegative) as diff-trees, where a
;; diff-tree is a list defined by the grammar
;;
;; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
;;
;; The list (one) represents 1. If t1 represents n1 and t2 represents n2, then (diff t1 t2) is a representation of
;; n1 - n2.
;;
;; So both (one) and (diff (one) (diff (one) (one))) are representations of 1; (diff (diff (one) (one)) (one)) is a
;; representation of -1.
;;
;; 1. Show that every number has infinitely many representations in this system.
;; 2. Turn this representation of the integers into an implementation by writing zero, is-zero?, successor, and
;;    predecessor, as specified on page 32, except that now the negative integers are also represented. Your procedures
;;    should take as input any of the multiple legal representations of an integer in this scheme. For example, if your
;;    successor procedure is given any of the infinitely many legal representations of 1, it should produce one of the
;;    legal representations of 2. It is permissible for different legal representations of 1 to yield different legal
;;    representations of 2.
;; 3. Write a procedure diff-tree-plus that does addition in this representation. Your procedure should be optimized
;;    for the diff-tree representation, and should do its work in a constant amount of time (independent of the size of
;;    its inputs). In particular, it should not be recursive.


(define zero
  (lambda ()
    '(diff (one) (one))))

(define interpret
  (lambda (n)
    (if (eqv? (car n) 'one)
        1
        (- (interpret (cadr n))
           (interpret (caddr n))))))

(define is-zero?
  (lambda (n)
    (zero? (interpret n))))

(define successor
  (lambda (n)
    (list 'diff n '(diff (diff (one) (one)) (one)))))

(define predecessor
  (lambda (n)
    (list 'diff n '(one))))

(define diff-tree-plus
  (lambda (m n)
    (list 'diff m (list 'diff '(diff (one) (one)) n))))

(provide zero is-zero? successor predecessor diff-tree-plus)
