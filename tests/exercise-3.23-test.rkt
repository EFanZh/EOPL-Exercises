#lang racket/base

(require rackunit)
(require "../solutions/exercise-3.x-proc-lang.rkt")

(check-equal? (run "let makemult = proc (maker)
                                     proc (x)
                                       if zero?(x)
                                       then 0
                                       else -(((maker maker) -(x, 1)), -4)
                    in let times4 = proc (x) ((makemult makemult) x)
                       in (times4 3)")
              (num-val 12))

(define (fact n)
  (run (format "(let maketimes = proc (maker)
                                   proc (x)
                                     proc (y)
                                       if zero?(x)
                                       then 0
                                       else -((((maker maker) -(x, 1)) y), -(0, y))
                 in let times = (maketimes maketimes)
                    in let makefact = proc (maker)
                                        proc (x)
                                          if zero?(x)
                                          then 1
                                          else ((times x) ((maker maker) -(x, 1)))
                       in (makefact makefact)
                 ~a)" n)))

(check-equal? (fact 0) (num-val 1))
(check-equal? (fact 1) (num-val 1))
(check-equal? (fact 2) (num-val 2))
(check-equal? (fact 3) (num-val 6))
(check-equal? (fact 4) (num-val 24))
(check-equal? (fact 5) (num-val 120))
(check-equal? (fact 6) (num-val 720))
(check-equal? (fact 7) (num-val 5040))
(check-equal? (fact 8) (num-val 40320))
(check-equal? (fact 9) (num-val 362880))
(check-equal? (fact 10) (num-val 3628800))
