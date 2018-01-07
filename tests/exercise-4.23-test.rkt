#lang racket

(require rackunit)
(require "../solutions/exercise-4.x-statement-oriented-implicit-refs-lang.rkt")

(define (get-output string input)
  (with-input-from-string input
    (λ ()
      (with-output-to-string
          (λ ()
            (run string))))))

(check-equal? (get-output "var x, y;
                           {
                             read x;
                             read y;
                             print -(x, y)
                           }"
                          "7 4")
              "3\n")
