#lang racket

(require rackunit)
(require "../solutions/exercise-3.x-proc-lang.rkt")

(define (get-result string)
  (let* ([result 'uninitialized]
         (output (with-output-to-string (λ ()
                                          (set! result (run string))))))
    (cons result output)))

(check-equal? (get-result "(let x = 5
                            in let y = 4
                               in let x = 3
                                  in traceproc (z)
                                       -(z, x)
                            9)")
              (cons (num-val 6)
                    "Entering procedure.\nExiting procedure.\n"))
