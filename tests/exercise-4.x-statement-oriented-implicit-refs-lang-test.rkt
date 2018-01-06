#lang racket

(require rackunit)
(require "../solutions/exercise-4.x-statement-oriented-implicit-refs-lang.rkt")

(define (get-output string)
  (with-output-to-string (λ ()
                           (run string))))

(check-equal? (get-output "print -(3, 3)") "0\n")
(check-equal? (get-output "print -(3, 4)") "-1\n")
(check-equal? (get-output "print -(4, 3)") "1\n")
(check-equal? (get-output "print zero?(0)") "#t\n")
(check-equal? (get-output "print zero?(4)") "#f\n")
(check-equal? (get-output "print if zero?(0) then 7 else 11") "7\n")
(check-equal? (get-output "print if zero?(2) then 7 else 11") "11\n")
(check-equal? (get-output "print let x = 5 in x") "5\n")
(check-equal? (get-output "print let x = 5 in let x = 3 in x") "3\n")

(check-equal? (get-output "print let f = proc (x) -(x, 11)
                                 in (f (f 77))")
              "55\n")

(check-equal? (get-output "print (proc (f) (f (f 77))
                                  proc (x) -(x, 11))")
              "55\n")

(check-equal? (get-output "print let x = 200
                                 in let f = proc (z)
                                              -(z, x)
                                    in let x = 100
                                       in let g = proc (z)
                                                    -(z, x)
                                          in -((f 1), (g 1))")
              "-100\n")

(check-equal? (get-output "print letrec double(x) = if zero?(x)
                                                    then 0
                                                    else -((double -(x, 1)), -2)
                                 in (double 6)")
              "12\n")

(check-equal? (get-output "print let x = 0
                                 in letrec even(dummy) = if zero?(x)
                                                         then 1
                                                         else begin set x = -(x, 1);
                                                                    (odd 888)
                                                              end
                                           odd(dummy) = if zero?(x)
                                                        then 0
                                                        else begin set x = -(x, 1);
                                                                   (even 888)
                                                             end
                                    in begin set x = 13;
                                             (odd 888)
                                       end")
              "1\n")

(check-equal? (get-output "print let g = let counter = 0
                                         in proc (dummy)
                                              begin set counter = -(counter, -1);
                                                    counter
                                              end
                                 in let a = (g 11)
                                    in let b = (g 11)
                                       in -(a, b)")
              "-1\n")
