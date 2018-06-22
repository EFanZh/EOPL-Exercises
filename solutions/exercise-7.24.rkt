#lang eopl

;; Exercise 7.24 [★★] Extend the inferencer to handle multiple let declarations, multiargument procedures, and
;; multiple letrec declarations.

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
    [expression ("proc" "(" (separated-list identifier ":" optional-type ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno optional-type identifier "(" (separated-list identifier ":" optional-type ",") ")" "="
                                 expression)
                          "in" expression)
                letrec-exp]
    [optional-type ("?") no-type]
    [optional-type (type) a-type]
    [type ("int") int-type]
    [type ("bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]
    [type ("%tvar-type" number) tvar-type]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

(define proc-type?
  (lambda (ty)
    (cases type ty
      [proc-type (ts t2) #t]
      [else #f])))

(define tvar-type?
  (lambda (ty)
    (cases type ty
      [tvar-type (serial-number) #t]
      [else #f])))

(define proc-type->arg-types
  (lambda (ty)
    (cases type ty
      [proc-type (arg-types result-type) arg-types]
      [else (eopl:error 'proc-type->arg-type "Not a proc type: ~s" ty)])))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      [proc-type (arg-types result-type) result-type]
      [else (eopl:error 'proc-type->result-types "Not a proc type: ~s" ty)])))

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
                                                 (list '-> (type-to-external-form result-type)))]
      [tvar-type (serial-number) (string->symbol (string-append "tvar" (number->string serial-number)))])))

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

(define extend-env*
  (lambda (vars vals saved-env)
    (let loop ([env saved-env]
               [vars vars]
               [vals vals])
      (if (null? vars)
          env
          (loop (extend-env (car vars) (car vals) env)
                (cdr vars)
                (cdr vals))))))

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

;; Data structures - type environment.

(define-datatype type-environment type-environment?
  [empty-tenv-record]
  [extended-tenv-record [sym symbol?]
                        [type type?]
                        [tenv type-environment?]])

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define extend-tenv*
  (lambda (syms types tenv)
    (let loop ([tenv tenv]
               [syms syms]
               [types types])
      (if (null? syms)
          tenv
          (loop (extend-tenv (car syms) (car types) tenv)
                (cdr syms)
                (cdr types))))))

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      [empty-tenv-record () (eopl:error 'apply-tenv "Unbound variable ~s" sym)]
      [extended-tenv-record (sym1 val1 old-env) (if (eqv? sym sym1)
                                                    val1
                                                    (apply-tenv old-env sym))])))

;; Data structures - substitution.

(define pair-of
  (lambda (pred1 pred2)
    (lambda (val)
      (and (pair? val) (pred1 (car val)) (pred2 (cdr val))))))

(define substitution?
  (list-of (pair-of tvar-type? type?)))

(define empty-subst
  (lambda ()
    '()))

(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      [int-type () (int-type)]
      [bool-type () (bool-type)]
      [proc-type (arg-types result-type) (proc-type (map (lambda (arg-type)
                                                           (apply-one-subst arg-type tvar ty1))
                                                         arg-types)
                                                    (apply-one-subst result-type tvar ty1))]
      [tvar-type (sn) (if (equal? ty0 tvar) ty1 ty0)])))

(define extend-subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (p)
                 (let ([oldlhs (car p)]
                       [oldrhs (cdr p)])
                   (cons oldlhs (apply-one-subst oldrhs tvar ty))))
               subst))))

(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      [int-type () (int-type)]
      [bool-type () (bool-type)]
      [proc-type (ts t2) (proc-type (map (lambda (t1)
                                           (apply-subst-to-type t1 subst))
                                         ts)
                                    (apply-subst-to-type t2 subst))]
      [tvar-type (sn) (let ([tmp (assoc ty subst)])
                        (if tmp
                            (cdr tmp)
                            ty))])))

;; Data structures - answer.

(define-datatype answer answer?
  [an-answer [type type?]
             [subst substitution?]])

;; Unifier.

(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      [int-type () #t]
      [bool-type () #t]
      [proc-type (arg-types result-type) (let loop ([arg-types arg-types])
                                           (if (null? arg-types)
                                               (no-occurrence? tvar result-type)
                                               (and (no-occurrence? tvar (car arg-types))
                                                    (loop (cdr arg-types)))))]
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
  (lambda (ty1 ty2 subst exp)
    (let ([ty1 (apply-subst-to-type ty1 subst)]
          [ty2 (apply-subst-to-type ty2 subst)])
      (cond [(equal? ty1 ty2) subst]
            [(tvar-type? ty1) (if (no-occurrence? ty1 ty2)
                                  (extend-subst subst ty1 ty2)
                                  (report-no-occurrence-violation ty1 ty2 exp))]
            [(tvar-type? ty2) (if (no-occurrence? ty2 ty1)
                                  (extend-subst subst ty2 ty1)
                                  (report-no-occurrence-violation ty2 ty1 exp))]
            [(and (proc-type? ty1) (proc-type? ty2)) (let ([subst (let loop ([subst subst]
                                                                             [types1 (proc-type->arg-types ty1)]
                                                                             [types2 (proc-type->arg-types ty2)])
                                                                    (if (null? types1)
                                                                        (if (null? types2)
                                                                            subst
                                                                            (eopl:error 'unifier
                                                                                        "Argument number not match"))
                                                                        (if (null? types2)
                                                                            (eopl:error 'unifier
                                                                                        "Argument number not match")
                                                                            (loop (unifier (car types1)
                                                                                           (car types2)
                                                                                           subst
                                                                                           exp)
                                                                                  (cdr types1)
                                                                                  (cdr types2)))))])
                                                       (let ([subst (unifier (proc-type->result-type ty1)
                                                                             (proc-type->result-type ty2)
                                                                             subst
                                                                             exp)])
                                                         subst))]
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
  (lambda (exp tenv subst)
    (cases expression exp
      [const-exp (num) (an-answer (int-type) subst)]
      [zero?-exp (exp1) (cases answer (type-of exp1 tenv subst)
                          [an-answer (type1 subst1) (let ([subst2 (unifier type1 (int-type) subst1 exp)])
                                                      (an-answer (bool-type) subst2))])]
      [diff-exp (exp1 exp2) (cases answer (type-of exp1 tenv subst)
                              [an-answer (type1 subst1) (let ([subst1 (unifier type1 (int-type) subst1 exp1)])
                                                          (cases answer (type-of exp2 tenv subst1)
                                                            [an-answer (type2 subst2) (let ([subst2 (unifier type2
                                                                                                             (int-type)
                                                                                                             subst2
                                                                                                             exp2)])
                                                                                        (an-answer (int-type)
                                                                                                   subst2))]))])]
      [if-exp (exp1 exp2 exp3)
              (cases answer (type-of exp1 tenv subst)
                [an-answer (ty1 subst) (let ([subst (unifier ty1 (bool-type) subst exp1)])
                                         (cases answer (type-of exp2 tenv subst)
                                           [an-answer (ty2 subst) (cases answer (type-of exp3 tenv subst)
                                                                    [an-answer (ty3 subst) (let ([subst (unifier ty2
                                                                                                                 ty3
                                                                                                                 subst
                                                                                                                 exp)])
                                                                                             (an-answer ty2
                                                                                                        subst))])]))])]
      [var-exp (var) (an-answer (apply-tenv tenv var) subst)]
      [let-exp (vars exps body) (let loop ([acc-rhs-types '()]
                                           [subst subst]
                                           [exps exps])
                                  (if (null? exps)
                                      (type-of body (extend-tenv* vars (reverse acc-rhs-types) tenv) subst)
                                      (cases answer (type-of (car exps) tenv subst)
                                        [an-answer (rhs-type subst) (loop (cons rhs-type acc-rhs-types)
                                                                          subst
                                                                          (cdr exps))])))]
      [proc-exp (vars otypes body) (let ([arg-types (map otype->type otypes)])
                                     (cases answer (type-of body (extend-tenv* vars arg-types tenv) subst)
                                       [an-answer (result-type subst)
                                                  (an-answer (proc-type arg-types result-type) subst)]))]
      [call-exp (rator rands)
                (let ([result-type (fresh-tvar-type)])
                  (cases answer (type-of rator tenv subst)
                    [an-answer (rator-type subst)
                               (let loop ([acc-rand-types '()]
                                          [subst subst]
                                          [rands rands])
                                 (if (null? rands)
                                     (let ([subst (unifier rator-type
                                                           (proc-type (reverse acc-rand-types) result-type)
                                                           subst
                                                           exp)])
                                       (an-answer result-type subst))
                                     (cases answer (type-of (car rands) tenv subst)
                                       [an-answer (rand-type subst) (loop (cons rand-type acc-rand-types)
                                                                          subst
                                                                          (cdr rands))])))]))]
      [letrec-exp (proc-result-otypes proc-names bvars proc-arg-otypes proc-bodies letrec-body)
                  (let ([proc-result-types (map otype->type proc-result-otypes)]
                        [proc-arg-types (map (lambda (proc-arg-otypes)
                                               (map otype->type proc-arg-otypes))
                                             proc-arg-otypes)])
                    (let ([tenv-for-letrec-body (extend-tenv* proc-names
                                                              (map proc-type proc-arg-types proc-result-types)
                                                              tenv)])
                      (let loop ([subst subst]
                                 [proc-result-types proc-result-types]
                                 [bvars bvars]
                                 [proc-bodies proc-bodies])
                        (if (null? bvars)
                            (type-of letrec-body tenv-for-letrec-body subst)
                            (cases answer (type-of (car proc-bodies)
                                                   (extend-tenv* (car bvars) (car proc-arg-types) tenv-for-letrec-body)
                                                   subst)
                              [an-answer (proc-body-type subst) (let ([subst (unifier proc-body-type
                                                                                      (car proc-result-types)
                                                                                      subst
                                                                                      (car proc-bodies))])
                                                                  (loop subst
                                                                        (cdr proc-result-types)
                                                                        (cdr bvars)
                                                                        (cdr proc-bodies)))])))))])))

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (cases answer (type-of exp1 (empty-tenv) (empty-subst))
                          [an-answer (ty subst) (apply-subst-to-type ty subst)])])))

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

(provide check num-val run)
