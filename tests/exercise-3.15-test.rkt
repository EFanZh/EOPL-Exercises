#lang racket/base

(require rackunit)
(require "../solutions/exercise-3.x-let-lang.rkt")

(define (check-result-and-output-equal? thunk expected-result expected-output)
  (let-values ([(result output) (let ([output-string-port (open-output-string)])
                                  (parameterize ([current-output-port output-string-port])
                                    (let* ([result (thunk)]
                                           [output (get-output-string output-string-port)])
                                      (values result output))))])
    (check-equal? result expected-result)
    (check-equal? output expected-output)))

(check-result-and-output-equal? (λ () (run "print(7)"))
                                (num-val 1)
                                "7\n")

(check-result-and-output-equal? (λ () (run "print(zero?(0))"))
                                (num-val 1)
                                "#t\n")

(check-result-and-output-equal? (λ () (run "print(zero?(7))"))
                                (num-val 1)
                                "#f\n")

(check-result-and-output-equal? (λ () (run "print(emptylist)"))
                                (num-val 1)
                                "()\n")

(check-result-and-output-equal? (λ () (run "print(list(3, 4, 6))"))
                                (num-val 1)
                                "(3 4 6)\n")
