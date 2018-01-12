#lang racket

(require rackunit)
(require "../solutions/exercise-4.x-mutable-pairs-lang.rkt")

(check-equal? (run "arraylength(newarray(1, 3))") (num-val 1))
(check-equal? (run "arraylength(newarray(2, 3))") (num-val 2))
(check-equal? (run "arraylength(newarray(3, 3))") (num-val 3))
(check-equal? (run "arraylength(newarray(4, 3))") (num-val 4))

(check-equal? (run "let a = newarray(3, 1)
                    in let x = 5
                       in arrayref(a, 2)")
              (num-val 1))

(check-exn exn:fail? (λ ()
                       (run "let a = newarray(3, 1)
                             in let x = 5
                                in arrayref(a, 3)")))
