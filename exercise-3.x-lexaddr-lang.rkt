#lang eopl

;; This code is a implementation of an extended version of the lexical addressing language.

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
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    [expression ("%nameless-var" number) nameless-var-exp]
    [expression ("%let" expression "in" expression) nameless-let-exp]
    [expression ("%lexproc" expression) nameless-proc-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Static environments.

(define empty-senv
  (lambda ()
    '()))

(define extend-senv
  (lambda (var senv)
    (cons var senv)))

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))

(define apply-senv
  (lambda (senv var)
    (cond [(null? senv) (report-unbound-var var)]
          [(eqv? var (car senv)) 0]
          [else (+ 1 (apply-senv (cdr senv) var))])))

;; Translator.

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'value-of "Illegal expression in source code: ~s" exp)))

(define translation-of
  (lambda (exp senv)
    (cases expression exp
      [const-exp (num) (const-exp num)]
      [diff-exp (exp1 exp2) (diff-exp (translation-of exp1 senv)
                                      (translation-of exp2 senv))]
      [zero?-exp (exp1) (zero?-exp (translation-of exp1 senv))]
      [if-exp (exp1 exp2 exp3) (if-exp (translation-of exp1 senv)
                                       (translation-of exp2 senv)
                                       (translation-of exp3 senv))]
      [var-exp (var) (nameless-var-exp (apply-senv senv var))]
      [let-exp (var exp1 body) (nameless-let-exp (translation-of exp1 senv)
                                                 (translation-of body (extend-senv var senv)))]
      [proc-exp (var body) (nameless-proc-exp (translation-of body (extend-senv var senv)))]
      [call-exp (rator rand) (call-exp (translation-of rator senv)
                                       (translation-of rand senv))]
      [else (report-invalid-source-expression exp)])))

;; Environments.

(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

;; Data structures.

(define-datatype proc proc?
  [procedure [body expression?]
             [env nameless-environment?]])

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]])

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors
                "Looking for a ~s, found ~s"
                variant
                value)))

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
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (body saved-env)
                 (value-of body (extend-nameless-env arg saved-env))))))

(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [diff-exp (exp1 exp2) (let ([val1 (expval->num (value-of exp1 nameless-env))]
                                  [val2 (expval->num (value-of exp2 nameless-env))])
                              (num-val (- val1 val2)))]
      [zero?-exp (exp1) (let ([val1 (expval->num (value-of exp1 nameless-env))])
                          (bool-val (zero? val1)))]
      [if-exp (exp0 exp1 exp2) (if (expval->bool (value-of exp0 nameless-env))
                                   (value-of exp1 nameless-env)
                                   (value-of exp2 nameless-env))]
      [call-exp (rator rand) (let ([proc (expval->proc (value-of rator nameless-env))]
                                   [arg (value-of rand nameless-env)])
                               (apply-procedure proc arg))]
      [nameless-var-exp (n) (apply-nameless-env nameless-env n)]
      [nameless-let-exp (exp1 body) (let ([val (value-of exp1 nameless-env)])
                                      (value-of body (extend-nameless-env val nameless-env)))]
      [nameless-proc-exp (body) (proc-val (procedure body nameless-env))]
      [else (eopl:error 'value-of "Illegal expression in translated code: ~s" exp)])))

;; Interfaces.

(define init-senv empty-senv)

(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (a-program (translation-of exp1 (init-senv)))])))

(define init-nameless-env empty-nameless-env)

(define value-of-translation
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (init-nameless-env))])))

(define run
  (lambda (string)
    (value-of-translation
     (translation-of-program
      (scan&parse string)))))

(provide num-val bool-val run)
