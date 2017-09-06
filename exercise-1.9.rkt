#lang eopl

;; Exercise 1.9 [★★] Define remove, which is like remove-first, except that it removes all occurrences of a given
;; symbol from a list of symbols, not just the first.

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

(provide remove)
