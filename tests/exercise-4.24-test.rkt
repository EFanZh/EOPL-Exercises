#lang racket

(require rackunit)
(require "../solutions/exercise-4.x-statement-oriented-implicit-refs-lang.rkt")

(define (get-output string)
      (with-output-to-string
          (λ ()
            (run string))))

(check-equal? (get-output "var x; {
                                    x = 0;
                                    do print x
                                    while not(zero?(x))
                                  }")
              "0\n")

(check-equal? (get-output "var x; {
                                    x = 5;
                                    do {
                                         print x;
                                         x = -(x, 1)
                                       }
                                    while not(zero?(x))
                                  }")
              "5\n4\n3\n2\n1\n")
