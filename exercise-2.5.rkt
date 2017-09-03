#lang eopl

;; Exercise 2.5 [🟉] We can use any data structure for representing environments, if we can distinguish empty
;; environments from non-empty ones, and in which one can extract the pieces of a non-empty environment. Implement
;; environments using a representation in which the empty environment is represented as the empty list, and in which
;; extend-env builds an environment that looks like
;;
;;          ┌───┬───┐
;;          │ ╷ │ ╶─┼─► saved-env
;;          └─┼─┴───┘
;;            ▼
;;          ┌───┬───┐
;;          │ ╷ │ ╷ │
;;          └─┼─┴─┼─┘
;;         ┌──┘   └───┐
;;         ▼          ▼
;;     saved-var  saved-val
;;
;; This is called an a-list or association-list representation.

(define empty-env
  (lambda ()
    '()))

(define apply-env
  (lambda (env var)
    (let ([head (car env)])
      (if (eqv? (car head) var)
          (cdr head)
          (apply-env (cdr env) var)))))

(define extend-env
  (lambda (var v env)
    (cons (cons var v) env)))

(provide empty-env apply-env extend-env)
