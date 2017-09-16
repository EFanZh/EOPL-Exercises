#lang eopl

;; Exercise 2.22 [★] Using define-datatype, implement the stack data type of exercise 2.4.

(define-datatype stack-type stack?
  (empty-stack)
  (push [saved-stack stack?]
        [val always?]))

(define pop
  (lambda (stack)
    (cases stack-type stack
      [empty-stack () (eopl:error 'pop "Can not pop an empty stack.")]
      [push (saved-stack val) saved-stack])))

(define top
  (lambda (stack)
    (cases stack-type stack
      [empty-stack () (eopl:error 'pop "Can not top an empty stack.")]
      [push (saved-stack val) val])))

(define empty-stack?
  (lambda (stack)
    (cases stack-type stack
      [empty-stack () #t]
      [push (saved-stack val) #f])))

(provide empty-stack push pop top empty-stack?)
