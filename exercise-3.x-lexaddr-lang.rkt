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
    [expression ("cond" (arbno expression "==>" expression) "end") cond-exp]
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("pack" "(" (separated-list expression ",") ")") pack-exp]
    [expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp]
    [expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("%nameless-var" number number) nameless-var-exp]
    [expression ("%let" expression "in" expression) nameless-let-exp]
    [expression ("%unpack" expression "in" expression) nameless-unpack-exp]
    [expression ("%letrec" expression "in" expression) nameless-letrec-exp]
    [expression ("%nameless-letrec-var" number number) nameless-letrec-var-exp]
    [expression ("%lexproc" "[" (arbno number number) "]" expression) nameless-proc-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Static environments.

(define-datatype static-environment static-environment?
  [empty-senv]
  [extend-senv [vars (list-of symbol?)]
               [saved-senv static-environment?]]
  [extend-senv-rec [vars (list-of symbol?)]
                   [saved-senv static-environment?]])

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))

(define-datatype lexical-address lexical-address?
  [normal-lex-addr [depth integer?]
                   [position integer?]]
  [letrec-lex-addr [depth integer?]
                   [position integer?]])

(define apply-senv
  (lambda (senv var)
    (let loop ([senv senv]
               [depth 0])
      (cases static-environment senv
        [empty-senv () (report-unbound-var var)]
        [extend-senv (saved-vars saved-senv) (let loop2 ([saved-vars saved-vars]
                                                         [position 0])
                                               (cond [(null? saved-vars) (loop saved-senv
                                                                               (+ depth 1))]
                                                     [(eqv? var (car saved-vars)) (normal-lex-addr depth position)]
                                                     [else (loop2 (cdr saved-vars)
                                                                  (+ position 1))]))]
        [extend-senv-rec (saved-vars saved-senv) (let loop2 ([saved-vars saved-vars]
                                                             [position 0])
                                                   (cond [(null? saved-vars) (loop saved-senv
                                                                                   (+ depth 1))]
                                                         [(eqv? var (car saved-vars)) (letrec-lex-addr depth position)]
                                                         [else (loop2 (cdr saved-vars)
                                                                      (+ position 1))]))]))))

(define (has-binding? senv var)
  (cases static-environment senv
    [empty-senv () #f]
    [extend-senv (saved-vars saved-senv) (or (memv var saved-vars)
                                             (has-binding? saved-senv var))]
    [extend-senv-rec (saved-vars saved-senv) (or (memv var saved-vars)
                                                 (has-binding? saved-senv var))]))

;; Translator.

(define (get-free-variables exp senv)
  (let helper ([exp exp]
               [senv senv]
               [free-variables '()])
    (cases expression exp
      [const-exp (num) free-variables]
      [diff-exp (exp1 exp2) (helper exp2
                                    senv
                                    (helper exp1
                                            senv
                                            free-variables))]
      [zero?-exp (exp1) (helper exp1 senv free-variables)]
      [if-exp (exp1 exp2 exp3) (helper exp3
                                       senv
                                       (helper exp2
                                               senv
                                               (helper exp1
                                                       senv
                                                       free-variables)))]
      [cond-exp (conditions results) (let loop ([conditions conditions]
                                                [results results]
                                                [free-variables free-variables])
                                       (if (null? conditions)
                                           free-variables
                                           (loop (cdr conditions)
                                                 (cdr results)
                                                 (helper (car results)
                                                         senv
                                                         (helper (car conditions)
                                                                 senv
                                                                 free-variables)))))]
      [var-exp (var) (if (or (memv var free-variables)
                             (has-binding? senv var))
                         free-variables
                         (cons var free-variables))]
      [let-exp (var exp1 body) (helper body
                                       (extend-senv (list var) senv)
                                       (helper exp1
                                               senv
                                               free-variables))]
      [pack-exp (items) (let loop ([items items]
                                   [free-variables free-variables])
                          (if (null? items)
                              free-variables
                              (loop (cdr items)
                                    (helper (car items)
                                            senv
                                            free-variables))))]
      [unpack-exp (vars exp body) (helper body
                                          (extend-senv vars senv)
                                          free-variables)]
      [letrec-exp (p-name b-var p-body letrec-body) (let ([rec-senv (extend-senv (list p-name) senv)])
                                                      (helper letrec-body
                                                              rec-senv
                                                              (helper p-body
                                                                      (extend-senv (list b-var) rec-senv)
                                                                      free-variables)))]
      [proc-exp (vars body) (helper body
                                    (extend-senv vars senv)
                                    free-variables)]
      [call-exp (rator rands) (let loop ([rands rands]
                                         [free-variables (helper rator
                                                                 senv
                                                                 free-variables)])
                                (if (null? rands)
                                    free-variables
                                    (loop (cdr rands)
                                          (helper (car rands)
                                                  senv
                                                  free-variables))))]
      [else (report-invalid-source-expression exp)])))

(define (get-addresses senv variables)
  (let loop ([depths '()]
             [positions '()]
             [variables variables])
    (if (null? variables)
        (cons (reverse depths)
              (reverse positions))
        (cases lexical-address (apply-senv senv (car variables))
          [normal-lex-addr (depth position) (loop (cons depth depths)
                                                  (cons position positions)
                                                  (cdr variables))]
          [letrec-lex-addr (depth position) (loop (cons depth depths)
                                                  (cons position positions)
                                                  (cdr variables))]))))

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
      [cond-exp (conditions results) (cond-exp (map (lambda (exp)
                                                      (translation-of exp senv))
                                                    conditions)
                                               (map (lambda (exp)
                                                      (translation-of exp senv))
                                                    results))]
      [var-exp (var) (cases lexical-address (apply-senv senv var)
                       [normal-lex-addr (depth position) (nameless-var-exp depth position)]
                       [letrec-lex-addr (depth position) (nameless-letrec-var-exp depth position)])]
      [let-exp (var exp1 body) (nameless-let-exp (translation-of exp1 senv)
                                                 (translation-of body (extend-senv (list var) senv)))]
      [pack-exp (items) (pack-exp (map (lambda (exp)
                                         (translation-of exp senv))
                                       items))]
      [unpack-exp (vars exp body) (nameless-unpack-exp (translation-of exp senv)
                                                       (translation-of body (extend-senv vars senv)))]
      [letrec-exp (p-name b-var p-body letrec-body) (let ([rec-env (extend-senv-rec (list p-name) senv)])
                                                      (nameless-letrec-exp (translation-of p-body
                                                                                           (extend-senv (list b-var)
                                                                                                        rec-env))
                                                                           (translation-of letrec-body rec-env)))]
      [proc-exp (vars body) (let* ([captured-variables (get-free-variables exp (empty-senv))]
                                   [addresses (get-addresses senv captured-variables)]
                                   [depths (car addresses)]
                                   [positions (cdr addresses)]
                                   [new-body-env (extend-senv vars (extend-senv captured-variables (empty-senv)))])
                              (nameless-proc-exp depths
                                                 positions
                                                 (translation-of body new-body-env)))]
      [call-exp (rator rands) (call-exp (translation-of rator senv)
                                        (map (lambda (rand)
                                               (translation-of rand senv))
                                             rands))]
      [else (report-invalid-source-expression exp)])))

;; Environments.

(define nameless-environment?
  (lambda (x)
    ((list-of (list-of expval?)) x)))

(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (vals nameless-env)
    (cons vals nameless-env)))

(define apply-nameless-env
  (lambda (nameless-env depth position)
    (list-ref (list-ref nameless-env depth) position)))

(define (parent-nameless-env nameless-env n)
  (list-tail nameless-env n))

;; Data structures.

(define-datatype proc proc?
  [procedure [body expression?]
             [env nameless-environment?]])

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]]
  [pack-val [items (list-of expval?)]])

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

(define expval->pack
  (lambda (v)
    (cases expval v
      [pack-val (items) items]
      [else (expval-extractor-error 'proc v)])))

;; Interpreter.

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      (procedure (body saved-env)
                 (value-of body (extend-nameless-env args saved-env))))))

(define (get-values nameless-env depths positions)
  (let loop ([depths depths]
             [positions positions]
             [values '()])
    (if (null? depths)
        (reverse values)
        (loop (cdr depths)
              (cdr positions)
              (cons (apply-nameless-env nameless-env
                                        (car depths)
                                        (car positions))
                    values)))))

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
      [cond-exp (conditions results) (let loop ([conditions conditions]
                                                [results results])
                                       (cond [(null? conditions) (eopl:error 'value-of "All cond tests failed.")]
                                             [(expval->bool (value-of (car conditions) nameless-env))
                                              (value-of (car results) nameless-env)]
                                             [else (loop (cdr conditions)
                                                         (cdr results))]))]
      [call-exp (rator rands) (let ([proc (expval->proc (value-of rator nameless-env))]
                                    [args (map (lambda (rand)
                                                 (value-of rand nameless-env))
                                               rands)])
                                (apply-procedure proc args))]
      [nameless-var-exp (depth position) (apply-nameless-env nameless-env depth position)]
      [nameless-letrec-var-exp (depth position) (let ([partial-proc (expval->proc (apply-nameless-env nameless-env
                                                                                                      depth
                                                                                                      position))]
                                                      [real-env (parent-nameless-env nameless-env depth)])
                                                  (cases proc partial-proc
                                                    [procedure (body unused-env) (proc-val (procedure body
                                                                                                      real-env))]))]
      [nameless-let-exp (exp1 body) (let ([val (value-of exp1 nameless-env)])
                                      (value-of body (extend-nameless-env (list val) nameless-env)))]
      [pack-exp (exps) (pack-val (map (lambda (exp)
                                        (value-of exp nameless-env))
                                      exps))]
      [nameless-unpack-exp (exp body) (let ([vals (expval->pack (value-of exp nameless-env))])
                                        (value-of body (extend-nameless-env vals nameless-env)))]
      [nameless-letrec-exp (p-body letrec-body) (let ([partial-proc (proc-val (procedure p-body (empty-nameless-env)))])
                                                  (value-of letrec-body
                                                            (extend-nameless-env (list partial-proc) nameless-env)))]
      [nameless-proc-exp (depths positions body) (proc-val (procedure body
                                                                      (extend-nameless-env (get-values nameless-env
                                                                                                       depths
                                                                                                       positions)
                                                                                           (empty-nameless-env))))]
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
