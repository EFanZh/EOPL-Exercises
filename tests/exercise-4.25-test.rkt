#lang racket

(require rackunit)
(require "../solutions/exercise-4.25.rkt")

(define (get-output string)
      (with-output-to-string
          (λ ()
            (run string))))

(check-equal? (get-output "var x = 8, y = 3;
                           print -(x, y)")
              "5\n")

(check-equal? (get-output "var x = 8, y = 3;
                           var x = y, y = x;
                           print -(x, y)")
              "-5\n")
