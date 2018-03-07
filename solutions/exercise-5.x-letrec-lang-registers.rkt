#lang eopl

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
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression)
                letrec-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures.

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

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval expval?]
              [saved-env environment?]]
  [extend-env-rec [p-name symbol?]
                  [b-vars (list-of symbol?)]
                  [p-body expression?]
                  [saved-env environment?]])

(define identifier? symbol?)

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont [saved-cont continuation?]]
  [let-exp-cont [var identifier?]
                [body expression?]
                [saved-env environment?]
                [saved-cont continuation?]]
  [if-test-cont [exp2 expression?]
                [exp3 expression?]
                [saved-env environment?]
                [saved-cont continuation?]]
  [diff1-cont [exp2 expression?]
              [saved-env environment?]
              [saved-cont continuation?]]
  [diff2-cont [val1 expval?]
              [saved-cont continuation?]]
  [rator-cont [rands (list-of expression?)]
              [saved-env environment?]
              [saved-cont continuation?]]
  [rand-cont [val1 expval?]
             [vals (list-of expval?)]
             [rands (list-of expression?)]
             [saved-env environment?]
             [saved-cont continuation?]])

;; Interpreter.

(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define vals 'uninitialized)
(define proc1 'uninitialized)

(define apply-procedure/k
  (lambda ()
    (cases proc proc1
      [procedure (vars body saved-env)
                 (set! exp body)
                 (set! env (let loop ([env saved-env]
                                      [vars vars]
                                      [vals vals])
                             (if (null? vars)
                                 env
                                 (loop (extend-env (car vars)
                                                   (car vals)
                                                   env)
                                       (cdr vars)
                                       (cdr vals)))))
                 (value-of/k)])))

(define used-end-conts '())

(define apply-cont
  (lambda ()
    (cases continuation cont
      [end-cont () (if (memq cont used-end-conts)
                       (eopl:error "Continuation is already used.")
                       (begin (set! used-end-conts (cons cont used-end-conts))
                              val))]
      [zero1-cont (saved-cont)
                  (set! cont saved-cont)
                  (set! val (bool-val (zero? (expval->num val))))
                  (apply-cont)]
      [let-exp-cont (var body saved-env saved-cont)
                    (set! cont saved-cont)
                    (set! exp body)
                    (set! env (extend-env var val saved-env))
                    (value-of/k)]
      [if-test-cont (exp2 exp3 saved-env saved-cont)
                    (set! cont saved-cont)
                    (if (expval->bool val)
                        (set! exp exp2)
                        (set! exp exp3))
                    (set! env saved-env)
                    (value-of/k)]
      [diff1-cont (exp2 saved-env saved-cont)
                  (set! cont (diff2-cont val saved-cont))
                  (set! exp exp2)
                  (set! env saved-env)
                  (value-of/k)]
      [diff2-cont (val1 saved-cont)
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val)])
                    (set! cont saved-cont)
                    (set! val (num-val (- num1 num2)))
                    (apply-cont))]
      [rator-cont (rands saved-env saved-cont)
                  (if (null? rands)
                      (let ([rator-proc (expval->proc val)])
                        (set! cont saved-cont)
                        (set! proc1 rator-proc)
                        (apply-procedure/k))
                      (begin (set! cont (rand-cont val
                                                   '()
                                                   (cdr rands)
                                                   saved-env
                                                   saved-cont))
                             (set! exp (car rands))
                             (set! env saved-env)
                             (value-of/k)))]
      [rand-cont (rator-val rand-vals rand-exps saved-env saved-cont)
                 (if (null? rand-exps)
                     (let ([rator-proc (expval->proc rator-val)])
                       (set! cont saved-cont)
                       (set! proc1 rator-proc)
                       (set! vals (reverse (cons val rand-vals)))
                       (apply-procedure/k))
                     (begin (set! cont (rand-cont rator-val
                                                  (cons val rand-vals)
                                                  (cdr rand-exps)
                                                  saved-env
                                                  saved-cont))
                            (set! exp (car rand-exps))
                            (set! env saved-env)
                            (value-of/k)))])))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (var val saved-env) (if (eqv? search-sym var)
                                          val
                                          (apply-env saved-env search-sym))]
      [extend-env-rec (p-name b-var p-body saved-env) (if (eqv? search-sym p-name)
                                                          (proc-val (procedure b-var p-body env))
                                                          (apply-env saved-env search-sym))])))

(define value-of/k
  (lambda ()
    (cases expression exp
      [const-exp (num)
                 (set! val (num-val num))
                 (apply-cont)]
      [var-exp (var)
               (set! val (apply-env env var))
               (apply-cont)]
      [proc-exp (vars body)
                (set! val (proc-val (procedure vars body env)))
                (apply-cont)]
      [letrec-exp (p-name b-vars p-body letrec-body)
                  (set! exp letrec-body)
                  (set! env (extend-env-rec p-name b-vars p-body env))
                  (value-of/k)]
      [zero?-exp (exp1)
                 (set! cont (zero1-cont cont))
                 (set! exp exp1)
                 (value-of/k)]
      [let-exp (var exp1 body)
               (set! cont (let-exp-cont var body env cont))
               (set! exp exp1)
               (value-of/k)]
      [if-exp (exp1 exp2 exp3)
              (set! cont (if-test-cont exp2 exp3 env cont))
              (set! exp exp1)
              (value-of/k)]
      [diff-exp (exp1 exp2)
                (set! cont (diff1-cont exp2 env cont))
                (set! exp exp1)
                (value-of/k)]
      [call-exp (rator rands)
                (set! cont (rator-cont rands env cont))
                (set! exp rator)
                (value-of/k)])))

(define (init-env)
  (empty-env))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (body)
                 (set! cont (end-cont))
                 (set! exp body)
                 (set! env (init-env))
                 (value-of/k)])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val num-val run)
