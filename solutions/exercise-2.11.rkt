#lang eopl

;; Exercise 2.11 [★★] A naive implementation of extend-env* from the preceding exercise requires time proportional to
;; k to run. It is possible to represent environments so that extend-env* requires only constant time: represent the
;; empty environment by the empty list, and represent a non-empty environment by the data structure
;;
;;           ┌───┬───┐
;;           │ ╷ │ ╶─┼─► saved-env
;;           └─┼─┴───┘
;;             ▼
;;           ┌───┬───┐
;;           │ ╷ │ ╷ │
;;           └─┼─┴─┼─┘
;;         ┌───┘   └───┐
;;         ▼           ▼
;;     saved-vars  saved-vals
;;
;; Such an environment might look like
;;
;;                    backbone
;;                       │
;;         ┌───┬───┐     ▼     ┌───┬───┐           ┌───┬───┐
;;         │ ╷ │ ╶─┼──────────►│ ╷ │ ╶─┼──────────►│ ╷ │ ╶─┼──────────► rest of environment
;;         └─┼─┴───┘           └─┼─┴───┘           └─┼─┴───┘
;;           ▼                   ▼                   ▼
;;         ┌───┬───┐           ┌───┬───┐           ┌───┬───┐
;;         │ ╷ │ ╷ │           │ ╷ │ ╷ │           │ ╷ │ ╷ │
;;         └─┼─┴─┼─┘           └─┼─┴─┼─┘           └─┼─┴─┼─┘
;;        ┌──┘   └──┐         ┌──┘   └──┐         ┌──┘   └──┐
;;        ▼         ▼         ▼         ▼         ▼         ▼
;;     (a b c)  (11 12 13)  (x z)    (66 77)    (x y)    (88 99)
;;
;; This is called the ribcage representation. The environment is represented as a list of pairs called ribs; each left
;; rib is a list of variables and each right rib is the corresponding list of values.
;;
;; Implement the environment interface, including extend-env*, in this representation.

(define empty-env
  (lambda ()
    '()))

(define apply-env
  (lambda (env search-var)
    (let loop ([env env])
      (let ([rib (car env)])
        (let apply-rib ([vars (car rib)]
                        [vals (cdr rib)])
          (cond [(null? vars) (loop (cdr env))]
                [(eqv? (car vars) search-var) (car vals)]
                [else (apply-rib (cdr vars) (cdr vals))]))))))

(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars vals) env)))

(define extend-env
  (lambda (var val env)
    (extend-env* (list var) (list val) env)))

(provide empty-env apply-env extend-env extend-env*)
