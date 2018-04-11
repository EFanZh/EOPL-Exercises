#lang racket

(require rackunit)
(require "../solutions/exercise-5.x-threads-lang.rkt")
(require "../solutions/exercise-5.51.rkt")

(let* ([result 'uninitialized]
       [output (with-output-to-string (λ ()
                                        (set! result (run program))))]
       [nums (map string->number (string-split output))])
  (check-equal? result (num-val 44))
  (check-equal? nums '(300 205 204 203 202 201)))
