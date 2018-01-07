#lang racket

(require rackunit)
(require "../solutions/exercise-4.26.rkt")

(define (get-output string)
      (with-output-to-string
          (λ ()
            (run string))))

(check-equal? (get-output "var even? = proc (num)
                                         if zero?(num)
                                         then 1
                                         else (odd? -(num, 1)),
                               odd? = proc (num)
                                        if zero?(num)
                                        then 0
                                        else (even? -(num, 1));
                           {
                             print (even? 0);
                             print (even? 1);
                             print (even? 2);
                             print (even? 3);
                             print (odd? 0);
                             print (odd? 1);
                             print (odd? 2);
                             print (odd? 3)
                           }")
              "1\n0\n1\n0\n0\n1\n0\n1\n")
