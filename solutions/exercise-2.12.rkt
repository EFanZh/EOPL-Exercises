#lang eopl

;; Exercise 2.12 [★] Implement the stack data type of exercise 2.4 using a procedural representation.

(define empty-stack
  (lambda ()
    (lambda (command)
      (cond [(eqv? command 'empty?) #t]))))

(define push
  (lambda (stack val)
    (lambda (command)
      (cond [(eqv? command 'empty?) #f]
            [(eqv? command 'pop) stack]
            [(eqv? command 'top) val]))))

(define pop
  (lambda (stack)
    (stack 'pop)))

(define top
  (lambda (stack)
    (stack 'top)))

(define empty-stack?
  (lambda (stack)
    (stack 'empty?)))

(provide empty-stack push pop top empty-stack?)
