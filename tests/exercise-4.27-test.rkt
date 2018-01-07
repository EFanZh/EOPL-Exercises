#lang racket

(require rackunit)
(require "../solutions/exercise-4.27.rkt")

(define (get-output string)
      (with-output-to-string
          (λ ()
            (run string))))

(check-equal? (get-output "var my-print = sub (num)
                                            print num;
                           call (my-print 13)")
              "13\n")

(check-equal? (get-output "var f = proc (x, y)
                                     -(x, y),
                               g = sub (num)
                                     print (f num 3);
                           call (g 6)")
              "3\n")
