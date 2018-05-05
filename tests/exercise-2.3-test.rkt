#lang racket/base

(require rackunit)
(require "../solutions/exercise-2.3.rkt")

(define interpret
  (lambda (n)
    (if (eqv? (car n) 'one)
        1
        (- (interpret (cadr n))
           (interpret (caddr n))))))

(define-binary-check (check-integer actual expected)
  (= (interpret actual) expected))

(define (from-integer n)
  (let loop ([result (zero)]
             [n n])
    (cond [(zero? n) result]
          [(negative? n) (loop (predecessor result) (+ n 1))]
          [else (loop (successor result) (- n 1))])))

(for ([i (in-range -100 100)])
  (check-integer (from-integer i) i))

(for ([i (in-range -20 20)]
      [j (in-range -20 20)])
  (check-integer (diff-tree-plus (from-integer i)
                                 (from-integer j))
                 (+ i j)))
