#lang eopl

;; Exercise 2.7 [🟉] Rewrite apply-env in figure 2.1 to give a more informative error message.

(define empty-env
  (lambda ()
    '(empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define apply-env
  (lambda (env search-var)
    (let loop ([env1 env])
      (cond
        [(eqv? (car env1) 'empty-env) (report-no-binding-found search-var env)]
        [(eqv? (car env1) 'extend-env) (let ([saved-var (cadr env1)]
                                             [saved-val (caddr env1)]
                                             [saved-env (cadddr env1)])
                                         (if (eqv? search-var saved-var)
                                             saved-val
                                             (loop saved-env)))]
        [else (report-invalid-env env1)]))))

(define collect-bindings
  (lambda (env)
    (let loop ([base '()]
               [env env])
      (let ([tag (car env)])
        (cond [(eqv? tag 'empty-env) base]
              [(eqv? tag 'extend-env) (let ([saved-var (cadr env)]
                                            [saved-val (caddr env)]
                                            [saved-env (cadddr env)])
                                        (loop (if (assv saved-var base)
                                                  base
                                                  (cons (cons saved-var saved-val) base))
                                              saved-env))])))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s. All bindings: ~s" search-var (collect-bindings env))))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(provide empty-env extend-env apply-env)
