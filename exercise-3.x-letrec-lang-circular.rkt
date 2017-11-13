#lang eopl

;; This code is a implementation of an extended version of LETREC language.

;; Grammar.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Environments.

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval (lambda (val)
                      (or (expval? val)
                          (vector? val)))]
              [saved-env environment?]])

(define extend-env-rec
  (lambda (p-names b-vars bodies saved-env)
    (let loop ([p-names p-names]
               [b-vars b-vars]
               [bodies bodies]
               [saved-env saved-env]
               [finalize-envs '()])
      (if (null? p-names)
          (let loop ([finalize-envs finalize-envs])
            (if (null? finalize-envs)
                saved-env
                (begin ((car finalize-envs) saved-env)
                       (loop (cdr finalize-envs)))))
          (let ([vec (make-vector 1)])
            (loop (cdr p-names)
                  (cdr b-vars)
                  (cdr bodies)
                  (extend-env (car p-names) vec saved-env)
                  (cons (lambda (env)
                          (vector-set! vec 0 (proc-val (procedure (car b-vars) (car bodies) env))))
                        finalize-envs)))))))

(define init-env empty-env)

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (var val saved-env) (if (eqv? search-sym var)
                                          (if (expval? val)
                                              val
                                              (vector-ref val 0))
                                          (apply-env saved-env search-sym))])))

;; Expressed values.

(define-datatype proc proc?
  [procedure [bvars (list-of symbol?)]
             [body expression?]
             [env environment?]])

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]])

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))

(define expval->num
  (lambda (v)
    (cases expval v
      [num-val (num) num]
      [else (expval-extractor-error 'num v)])))

(define expval->bool
  (lambda (v)
    (cases expval v
      [bool-val (bool) bool]
      [else (expval-extractor-error 'bool v)])))

(define expval->proc
  (lambda (v)
    (cases expval v
      [proc-val (proc) proc]
      [else (expval-extractor-error 'proc v)])))

;; Interpreter.

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      [procedure (vars body saved-env) (let loop ([env saved-env]
                                                  [vars vars]
                                                  [args args])
                                         (if (null? vars)
                                             (value-of body env)
                                             (loop (extend-env (car vars) (car args) env)
                                                   (cdr vars)
                                                   (cdr args))))])))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env env var)]
      [diff-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (let ([num1 (expval->num val1)]
                                    [num2 (expval->num val2)])
                                (num-val (- num1 num2))))]
      [zero?-exp (exp1) (let ([val1 (value-of exp1 env)])
                          (let ([num1 (expval->num val1)])
                            (if (zero? num1)
                                (bool-val #t)
                                (bool-val #f))))]
      [if-exp (exp1 exp2 exp3) (let ([val1 (value-of exp1 env)])
                                 (if (expval->bool val1)
                                     (value-of exp2 env)
                                     (value-of exp3 env)))]
      [let-exp (vars exps body) (let loop ([env env]
                                           [vars vars]
                                           [exps exps])
                                  (if (null? vars)
                                      (value-of body env)
                                      (loop (extend-env (car vars) (value-of (car exps) env) env)
                                            (cdr vars)
                                            (cdr exps))))]
      [proc-exp (vars body) (proc-val (procedure vars body env))]
      [call-exp (rator rands) (let ([proc (expval->proc (value-of rator env))]
                                    [args (map (lambda (rand) (value-of rand env)) rands)])
                                (apply-procedure proc args))]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (value-of letrec-body
                                                                  (extend-env-rec p-names b-vars p-bodies env))])))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (init-env))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; Provides.

(provide num-val bool-val run)
