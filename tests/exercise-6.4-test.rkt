#lang racket

(require rackunit)
(require "../solutions/exercise-6.4.rkt")

(define (get-result program)
  (let* ([result 'undefined]
         [output (with-output-to-string (lambda ()
                                          (set! result (program))))])
    (values output result)))

(define end-cont-output "End of computation.\nThis sentence should appear only once.\n")

(define (check-program reference others args expected-result)
  (check-equal? (apply reference args) expected-result)
  (for ([program others])
    (let-values ([(output result) (get-result (lambda ()
                                                (apply program args)))])
      (check-equal? result expected-result)
      (check-equal? output end-cont-output))))

;; remove-first.

(define (check-remove-first s los expected-result)
  (check-program remove-first
                 (list remove-first-data-structure
                       remove-first-procedural
                       remove-first-inlined-procedural
                       remove-first-registerized)
                 (list s los)
                 expected-result))

(check-remove-first 'a '(a b c) '(b c))
(check-remove-first 'b '(e f g) '(e f g))
(check-remove-first 'a4 '(c1 a4 c1 a4) '(c1 c1 a4))
(check-remove-first 'x '() '())

;; list-sum.

(define (check-list-sum loi expected-result)
  (check-program list-sum
                 (list list-sum-data-structure
                       list-sum-procedural
                       list-sum-inlined-procedural
                       list-sum-registerized)
                 (list loi)
                 expected-result))

(check-list-sum '() 0)
(check-list-sum '(0) 0)
(check-list-sum '(1) 1)
(check-list-sum '(2) 2)
(check-list-sum '(0 1) 1)
(check-list-sum '(0 1 2) 3)
(check-list-sum '(0 1 2 3) 6)
(check-list-sum '(0 1 2 3 4) 10)

;; occurs-free?.

(define (check-occurs-free? var exp expected-result)
  (check-program occurs-free?
                 (list occurs-free?-data-structure
                       occurs-free?-procedural
                       occurs-free?-inlined-procedural
                       occurs-free?-registerized)
                 (list var exp)
                 expected-result))

(check-occurs-free? 'x 'x #t)
(check-occurs-free? 'x 'y #f)
(check-occurs-free? 'x '(lambda (x) (x y)) #f)
(check-occurs-free? 'x '(lambda (y) (x y)) #t)
(check-occurs-free? 'x '((lambda (x) x) (x y)) #t)
(check-occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))) #t)

;; subst.

(define (check-subst new old slist expected-result)
  (check-program subst
                 (list subst-data-structure
                       subst-procedural
                       subst-inlined-procedural
                       subst-registerized)
                 (list new old slist)
                 expected-result))

(check-subst 'a 'b '((b c) (b () d)) '((a c) (a () d)))
(check-subst 'a 'b '() '())
(check-subst 'a 'b '(a) '(a))
(check-subst 'a 'b '(b) '(a))
(check-subst 'a 'b '(a a) '(a a))
(check-subst 'a 'b '(a b) '(a a))
(check-subst 'a 'b '(b a) '(a a))
(check-subst 'a 'b '(b b) '(a a))
