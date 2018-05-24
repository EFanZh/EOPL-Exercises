#lang eopl

;; Exercise 7.5 [★★] Extend the checker to handle multiple let declarations, multiargument procedures, and multiple
;; letrec declarations. You will need to add types of the form (t1 * t2 * ... * tn -> t) to handle multiargument
;; procedures.

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
    [expression ("proc" "(" (separated-list identifier ":" type ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno type identifier "(" (separated-list identifier ":" type ",") ")" "=" expression)
                          "in" expression)
                letrec-exp]
    [type ("int") int-type]
    [type ("bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures - expressed values.

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

;; Data structures - environment.

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval expval?]
              [saved-env environment?]]
  [extend-env-rec [p-names (list-of symbol?)]
                  [b-vars (list-of (list-of symbol?))]
                  [p-bodies (list-of expression?)]
                  [saved-env environment?]])

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (bvar bval saved-env) (if (eqv? search-sym bvar)
                                            bval
                                            (apply-env saved-env search-sym))]
      [extend-env-rec (p-names b-vars p-bodies saved-env)
                      (let loop ([p-names p-names]
                                 [b-vars b-vars]
                                 [p-bodies p-bodies])
                        (cond [(null? p-names) (apply-env saved-env search-sym)]
                              [(eqv? search-sym (car p-names)) (proc-val (procedure (car b-vars) (car p-bodies) env))]
                              [else (loop (cdr p-names)
                                          (cdr b-vars)
                                          (cdr p-bodies))]))])))

(define extend-env*
  (lambda (bvars bvals saved-env)
    (if (null? bvars)
        saved-env
        (extend-env* (cdr bvars)
                     (cdr bvals)
                     (extend-env (car bvars)
                                 (car bvals)
                                 saved-env)))))

;; Data structures - type environment.

(define-datatype type-environment type-environment?
  [empty-tenv-record]
  [extended-tenv-record [sym symbol?]
                        [type type?]
                        [tenv type-environment?]])

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      [empty-tenv-record () (eopl:error 'apply-tenv "Unbound variable ~s" sym)]
      [extended-tenv-record (sym1 val1 old-env) (if (eqv? sym sym1)
                                                    val1
                                                    (apply-tenv old-env sym))])))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define extend-tenv*
  (lambda (syms types saved-tenv)
    (if (null? syms)
        saved-tenv
        (extend-tenv* (cdr syms)
                      (cdr types)
                      (extend-tenv (car syms)
                                   (car types)
                                   saved-tenv)))))

;; Checker.

(define list-join
  (lambda (lst sep)
    (if (null? lst)
        '()
        (let loop ([acc (list (car lst))]
                   [lst (cdr lst)])
          (if (null? lst)
              (reverse acc)
              (loop (cons (car lst) (cons sep acc))
                    (cdr lst)))))))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      [int-type () 'int]
      [bool-type () 'bool]
      [proc-type (arg-types result-type) (append (list-join (map type-to-external-form arg-types) '*)
                                                 (list '-> (type-to-external-form result-type)))])))

(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
                "Types didn't match: ~s != ~a in~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp))))

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
                "Rator not a proc type:~%~s~%had rator type ~s"
                rator
                (type-to-external-form rator-type))))

(define type-of
  (lambda (exp tenv)
    (cases expression exp
      [const-exp (num) (int-type)]
      [var-exp (var) (apply-tenv tenv var)]
      [diff-exp (exp1 exp2) (let ([ty1 (type-of exp1 tenv)]
                                  [ty2 (type-of exp2 tenv)])
                              (check-equal-type! ty1 (int-type) exp1)
                              (check-equal-type! ty2 (int-type) exp2)
                              (int-type))]
      [zero?-exp (exp1) (let ([ty1 (type-of exp1 tenv)])
                          (check-equal-type! ty1 (int-type) exp1)
                          (bool-type))]
      [if-exp (exp1 exp2 exp3) (let ([ty1 (type-of exp1 tenv)]
                                     [ty2 (type-of exp2 tenv)]
                                     [ty3 (type-of exp3 tenv)])
                                 (check-equal-type! ty1 (bool-type) exp1)
                                 (check-equal-type! ty2 ty3 exp)
                                 ty2)]
      [let-exp (vars exps body) (let ([exp-types (map (lambda (exp1)
                                                        (type-of exp1 tenv))
                                                      exps)])
                                  (type-of body (extend-tenv* vars exp-types tenv)))]
      [proc-exp (vars var-types body) (let ([result-type (type-of body (extend-tenv* vars var-types tenv))])
                                        (proc-type var-types result-type))]
      [call-exp (rator rands) (let ([rator-type (type-of rator tenv)]
                                    [rand-types (map (lambda (rand)
                                                       (type-of rand tenv))
                                                     rands)])
                                (cases type rator-type
                                  [proc-type (arg-types result-type) (begin (for-each check-equal-type!
                                                                                      arg-types
                                                                                      rand-types
                                                                                      rands)
                                                                            result-type)]
                                  [else (report-rator-not-a-proc-type rator-type rator)]))]
      (letrec-exp (p-result-types p-names b-vars b-var-types p-bodies letrec-body)
                  (let ([tenv-for-letrec-body (extend-tenv* p-names
                                                            (map (lambda (b-var-type p-result-type)
                                                                   (proc-type b-var-type p-result-type))
                                                                 b-var-types
                                                                 p-result-types)
                                                            tenv)])
                    (let ([p-body-types (map (lambda (b-vars b-var-types p-body)
                                               (type-of p-body (extend-tenv* b-vars b-var-types tenv-for-letrec-body)))
                                             b-vars
                                             b-var-types
                                             p-bodies)])
                      (for-each check-equal-type! p-body-types p-result-types p-bodies)
                      (type-of letrec-body tenv-for-letrec-body)))))))

(define type-of-program
  (lambda (pgm)
    [cases program pgm (a-program (exp1) (type-of exp1 (empty-tenv)))]))

;; Interpreter.

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      [procedure (vars body saved-env) (value-of body (extend-env* vars args saved-env))])))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env env var)]
      [diff-exp (exp1 exp2) (let ([val1 (expval->num (value-of exp1 env))]
                                  [val2 (expval->num (value-of exp2 env))])
                              (num-val (- val1 val2)))]
      [zero?-exp (exp1) (let ([val1 (expval->num (value-of exp1 env))])
                          (if (zero? val1)
                              (bool-val #t)
                              (bool-val #f)))]
      [if-exp (exp0 exp1 exp2) (if (expval->bool (value-of exp0 env))
                                   (value-of exp1 env)
                                   (value-of exp2 env))]
      [let-exp (vars exps body) (let ([vals (map (lambda (exp1)
                                                   (value-of exp1 env))
                                                 exps)])
                                  (value-of body (extend-env* vars vals env)))]
      [proc-exp (bvars tys body) (proc-val (procedure bvars body env))]
      [call-exp (rator rands) (let ([proc (expval->proc (value-of rator env))]
                                    [args (map (lambda (rand)
                                                 (value-of rand env))
                                               rands)])
                                (apply-procedure proc args))]
      [letrec-exp (tys1 p-names b-vars tys2 p-bodies letrec-body) (value-of letrec-body
                                                                            (extend-env-rec p-names
                                                                                            b-vars
                                                                                            p-bodies
                                                                                            env))])))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (body) (value-of body (empty-env))])))

;; Interface.

(define check
  (lambda (string)
    (type-to-external-form (type-of-program (scan&parse string)))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val check num-val run)
