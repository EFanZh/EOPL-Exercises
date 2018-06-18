#lang eopl

;; Exercise 7.21 [★★] We said the substitution is like a store. Implement the unifier, using the representation of
;; substitutions from exercise 7.17, and keeping the substitution in a global Scheme variable, as we did in figures 4.1
;; and 4.2.

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
    [expression ("proc" "(" identifier ":" optional-type ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    [expression ("letrec" optional-type identifier "(" identifier ":" optional-type ")" "=" expression "in" expression)
                letrec-exp]
    [optional-type ("?") no-type]
    [optional-type (type) a-type]
    [type ("int") int-type]
    [type ("bool") bool-type]
    [type ("(" type "->" type ")") proc-type]
    [type ("%tvar-type" number) tvar-type]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

(define proc-type?
  (lambda (ty)
    (cases type ty
      [proc-type (t1 t2) #t]
      [else #f])))

(define tvar-type?
  (lambda (ty)
    (cases type ty
      [tvar-type (serial-number) #t]
      [else #f])))

(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      [proc-type (arg-type result-type) arg-type]
      [else (eopl:error 'proc-type->arg-type "Not a proc type: ~s" ty)])))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      [proc-type (arg-type result-type) result-type]
      [else (eopl:error 'proc-type->result-types "Not a proc type: ~s" ty)])))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      [int-type () 'int]
      [bool-type () 'bool]
      [proc-type (arg-type result-type) (list (type-to-external-form arg-type) '-> (type-to-external-form result-type))]
      [tvar-type (serial-number) (string->symbol (string-append "tvar" (number->string serial-number)))])))

;; Data structures - expressed values.

(define-datatype proc proc?
  [procedure [bvar symbol?]
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
  [extend-env-rec [p-name symbol?]
                  [b-var symbol?]
                  [p-body expression?]
                  [saved-env environment?]])

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (bvar bval saved-env) (if (eqv? search-sym bvar)
                                            bval
                                            (apply-env saved-env search-sym))]
      [extend-env-rec (p-name b-var p-body saved-env) (if (eqv? search-sym p-name)
                                                          (proc-val (procedure b-var p-body env))
                                                          (apply-env saved-env search-sym))])))

;; Data structures - type environment.

(define-datatype type-environment type-environment?
  [empty-tenv-record]
  [extended-tenv-record [sym symbol?]
                        [type type?]
                        [tenv type-environment?]])

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      [empty-tenv-record () (eopl:error 'apply-tenv "Unbound variable ~s" sym)]
      [extended-tenv-record (sym1 val1 old-env) (if (eqv? sym sym1)
                                                    val1
                                                    (apply-tenv old-env sym))])))

;; Data structures - substitution.

(define the-substitution 'uninitialized)

(define pair-of
  (lambda (pred1 pred2)
    (lambda (val)
      (and (pair? val) (pred1 (car val)) (pred2 (cdr val))))))

(define substitution?
  (list-of (pair-of tvar-type? type?)))

(define empty-subst
  (lambda ()
    '()))

(define initialize-substitution!
  (lambda ()
    (set! the-substitution (empty-subst))))

(define extend-subst
  (lambda (tvar ty)
    (set! the-substitution (cons (cons tvar ty) the-substitution))))

(define apply-subst-to-type
  (lambda (ty)
    (cases type ty
      [int-type () (int-type)]
      [bool-type () (bool-type)]
      [proc-type (t1 t2) (proc-type (apply-subst-to-type t1) (apply-subst-to-type t2))]
      [tvar-type (sn) (let ([tmp (assoc ty the-substitution)])
                        (if tmp
                            (apply-subst-to-type (cdr tmp))
                            ty))])))

;; Data structures - answer.

(define-datatype answer answer?
  [an-answer [type type?]])

;; Unifier.

(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      [int-type () #t]
      [bool-type () #t]
      [proc-type (arg-type result-type) (and (no-occurrence? tvar arg-type)
                                             (no-occurrence? tvar result-type))]
      [tvar-type (serial-number) (not (equal? tvar ty))])))

(define report-no-occurrence-violation
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-no-occurence!
                "Can't unify: type variable ~s occurs in type ~s in expression ~s~%"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

(define report-unification-failure
  (lambda (ty1 ty2 exp)
    (eopl:error 'unification-failure
                "Type mismatch: ~s doesn't match ~s in ~s~%"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

(define unifier
  (lambda (ty1 ty2 exp)
    (let ([ty1 (apply-subst-to-type ty1)]
          [ty2 (apply-subst-to-type ty2)])
      (cond [(equal? ty1 ty2) #f]
            [(tvar-type? ty1) (if (no-occurrence? ty1 ty2)
                                  (extend-subst ty1 ty2)
                                  (report-no-occurrence-violation ty1 ty2 exp))]
            [(tvar-type? ty2) (if (no-occurrence? ty2 ty1)
                                  (extend-subst ty2 ty1)
                                  (report-no-occurrence-violation ty2 ty1 exp))]
            [(and (proc-type? ty1) (proc-type? ty2)) (unifier (proc-type->arg-type ty1)
                                                              (proc-type->arg-type ty2)
                                                              exp)
                                                     (unifier (proc-type->result-type ty1)
                                                              (proc-type->result-type ty2)
                                                              exp)]
            [else (report-unification-failure ty1 ty2 exp)]))))

;; Inferrer.

(define sn 'uninitialized)

(define fresh-tvar-type
  (let ([sn 0])
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

(define otype->type
  (lambda (otype)
    (cases optional-type otype
      [no-type () (fresh-tvar-type)]
      [a-type (ty) ty])))

(define type-of
  (lambda (exp tenv)
    (cases expression exp
      [const-exp (num) (an-answer (int-type))]
      [zero?-exp (exp1)
                 (cases answer (type-of exp1 tenv)
                   [an-answer (type1) (unifier type1 (int-type) exp)])
                 (an-answer (bool-type))]
      [diff-exp (exp1 exp2)
                (cases answer (type-of exp1 tenv)
                  [an-answer (type1) (unifier type1 (int-type) exp1)])
                (cases answer (type-of exp2 tenv)
                  [an-answer (type2) (unifier type2 (int-type) exp2)])
                (an-answer (int-type))]
      [if-exp (exp1 exp2 exp3)
              (cases answer (type-of exp1 tenv)
                [an-answer (ty1) (unifier ty1 (bool-type) exp1)])
              (cases answer (type-of exp2 tenv)
                [an-answer (ty2)
                           (cases answer (type-of exp3 tenv)
                             [an-answer (ty3) (unifier ty2 ty3 exp)])
                           (an-answer ty2)])]
      [var-exp (var) (an-answer (apply-tenv tenv var))]
      [let-exp (var exp1 body) (cases answer (type-of exp1 tenv)
                                 [an-answer (rhs-type) (type-of body (extend-tenv var rhs-type tenv))])]
      [proc-exp (var otype body) (let ([arg-type (otype->type otype)])
                                   (cases answer (type-of body (extend-tenv var arg-type tenv))
                                     [an-answer (result-type) (an-answer (proc-type arg-type result-type))]))]
      [call-exp (rator rand) (let ([result-type (fresh-tvar-type)])
                               (cases answer (type-of rator tenv)
                                 [an-answer (rator-type) (cases answer (type-of rand tenv)
                                                           [an-answer (rand-type)
                                                                      (unifier rator-type
                                                                               (proc-type rand-type result-type)
                                                                               exp)])])
                               (an-answer result-type))]
      [letrec-exp (proc-result-otype proc-name bvar proc-arg-otype proc-body letrec-body)
                  (let ([proc-result-type (otype->type proc-result-otype)]
                        [proc-arg-type (otype->type proc-arg-otype)])
                    (let ([tenv-for-letrec-body (extend-tenv proc-name
                                                             (proc-type proc-arg-type proc-result-type)
                                                             tenv)])
                      (cases answer (type-of proc-body (extend-tenv bvar proc-arg-type tenv-for-letrec-body))
                        [an-answer (proc-body-type) (unifier proc-body-type proc-result-type proc-body)])
                      (type-of letrec-body tenv-for-letrec-body)))])))

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (cases answer (type-of exp1 (empty-tenv))
                          [an-answer (ty) (apply-subst-to-type ty)])])))

;; Interpreter.

(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      [procedure (var body saved-env) (value-of body (extend-env var arg saved-env))])))

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
      [let-exp (var exp1 body) (let ([val (value-of exp1 env)])
                                 (value-of body (extend-env var val env)))]
      [proc-exp (bvar ty body) (proc-val (procedure bvar body env))]
      [call-exp (rator rand) (let ([proc (expval->proc (value-of rator env))]
                                   [arg (value-of rand env)])
                               (apply-procedure proc arg))]
      [letrec-exp (ty1 p-name b-var ty2 p-body letrec-body) (value-of letrec-body
                                                                      (extend-env-rec p-name b-var p-body env))])))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (body) (value-of body (empty-env))])))

;; Interface.

(define check
  (lambda (string)
    (initialize-substitution!)
    (type-to-external-form (type-of-program (scan&parse string)))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide check num-val run)
