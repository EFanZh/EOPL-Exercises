#lang eopl

(define n1 'uninitialized)
(define a 'uninitialized)

(define (fact-iter-acc)
  (if (zero? n1)
      a
      (begin (set! a (* n1 a))
             (set! n1 (- n1 1))
             (fact-iter-acc))))

(define (fact-iter n)
  (set! n1 n)
  (set! a 1)
  (fact-iter-acc))

(provide fact-iter)
