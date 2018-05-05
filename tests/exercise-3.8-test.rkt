#lang racket/base

(require rackunit)
(require "../solutions/exercise-3.x-let-lang.rkt")

(check-equal? (run "equal?(3, 3)") (bool-val #t))
(check-equal? (run "equal?(3, 4)") (bool-val #f))
(check-equal? (run "equal?(4, 3)") (bool-val #f))
(check-equal? (run "greater?(3, 3)") (bool-val #f))
(check-equal? (run "greater?(3, 4)") (bool-val #f))
(check-equal? (run "greater?(4, 3)") (bool-val #t))
(check-equal? (run "less?(3, 3)") (bool-val #f))
(check-equal? (run "less?(3, 4)") (bool-val #t))
(check-equal? (run "less?(4, 3)") (bool-val #f))
