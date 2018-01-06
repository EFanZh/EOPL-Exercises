#lang racket

(require rackunit)
(require "../solutions/exercise-2.11.rkt")

(check-eqv? (apply-env (extend-env 'a 7 (empty-env))
                       'a)
            7)

(check-eqv? (apply-env (extend-env* '(a) '(7) (empty-env))
                       'a)
            7)

(let ([env (extend-env* '(a b) '(7 4) (empty-env))])
  (check-eqv? (apply-env env 'a) 7)
  (check-eqv? (apply-env env 'b) 4))

(let ([env (extend-env* '(a b c) '(7 4 12) (empty-env))])
  (check-eqv? (apply-env env 'a) 7)
  (check-eqv? (apply-env env 'b) 4)
  (check-eqv? (apply-env env 'c) 12))

(let ([env (extend-env* '(b d e)
                        '(7 4 12)
                        (extend-env* '(a b c)
                                     '(9 6 19)
                                     (empty-env)))])
  (check-eqv? (apply-env env 'a) 9)
  (check-eqv? (apply-env env 'b) 7)
  (check-eqv? (apply-env env 'c) 19)
  (check-eqv? (apply-env env 'd) 4)
  (check-eqv? (apply-env env 'e) 12))
