#lang eopl

;; Exercise 1.8 [🟉] In the definition of remove-first, if the last line were replaced by (remove-first s (cdr los)),
;; what function would the resulting procedure compute? Give the contract, including the usage statement, for the
;; revised procedure.

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-first s (cdr los))))))

(provide remove-first)
