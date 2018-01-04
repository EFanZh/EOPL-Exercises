#lang racket

(require rackunit)
(require "exercise-4.x-statement-oriented-implicit-refs-lang.rkt")

(define (get-output string)
  (with-output-to-string (λ ()
                           (run string))))

(check-equal? (get-output "var x, y;
                           {
                             x = 3;
                             y = 4;
                             print +(x, y)
                           }")
              "7\n")

(check-equal? (get-output "var x, y, z;
                           {
                             x = 3;
                             y = 4;
                             z = 0;
                             while not(zero?(x))
                             {
                               z = +(z, y);
                               x = -(x, 1)
                             };
                             print z
                           }")
              "12\n")

(check-equal? (get-output "var x;
                           {
                             x = 3;
                             print x;
                             var x;
                             {
                               x = 4;
                               print x
                             };
                             print x
                           }")
              "3\n4\n3\n")

(check-equal? (get-output "var f, x;
                           {
                             f = proc(x, y) *(x, y);
                             x = 3;
                             print (f 4 x)
                           }")
              "12\n")

(check-equal? (get-output "print 7") "7\n")
(check-equal? (get-output "print zero?(0)") "#t\n")
(check-equal? (get-output "print zero?(1)") "#f\n")
(check-equal? (get-output "print zero?(0)") "#t\n")
(check-equal? (get-output "print proc () 1") "<procedure>\n")

(check-equal? (get-output "{
                             print 3;
                             print 4
                           }")
              "3\n4\n")

(check-equal? (get-output "if zero?(0)
                           {
                             print 3
                           }
                           {
                             print 4
                           }")
              "3\n")

