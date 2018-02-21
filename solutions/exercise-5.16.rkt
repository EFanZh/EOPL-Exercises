#lang eopl

;; Exercise 5.16 [★★] Extend the continuation-passing interpreter to the language of exercise 4.22. Pass a
;; continuation argument to result-of, and make sure that no call to result-of occurs in a position that grows a control
;; context. Since a statement does not return a value, distinguish between ordinary continuations and continuations for
;; statements; the latter are usually called command continuations. The interface should include a procedure
;; apply-command-cont that takes a command continuation and invokes it. Implement command continuations both as data
;; structures and as zero-argument procedures.

;; Grammar.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([program (statement) a-program]
    [statement (identifier "=" expression) assign-statement]
    [statement ("print" expression) print-statement]
    [statement ("{" (separated-list statement ";") "}") brace-statement]
    [statement ("if" expression statement statement) if-statement]
    [statement ("while" expression statement) while-statement]
    [statement ("var" (separated-list identifier ",") ";" statement) block-statement]
    [expression (number) const-exp]
    [expression ("+" "(" expression "," expression ")") add-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("*" "(" expression "," expression ")") multiply-exp]
    [expression ("not" "(" expression ")") not-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp]
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("set" identifier "=" expression) assign-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

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

;; Environments.

(define reference?
  (lambda (v)
    (integer? v)))

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval reference?]
              [saved-env environment?]])

(define location
  (lambda (sym syms)
    (cond [(null? syms) #f]
          [(eqv? sym (car syms)) 0]
          [(location sym (cdr syms)) => (lambda (n)
                                          (+ n 1))]
          [else #f])))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-var)]
      [extend-env (bvar bval saved-env) (if (eqv? search-var bvar)
                                            bval
                                            (apply-env saved-env search-var))])))

;; Store.

(define the-store 'uninitialized)

(define (undefined-value)
  'undefined)

(define (undefined-value? value)
  (eqv? value 'undefined))

(define empty-store
  (lambda ()
    '()))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define newref
  (lambda (val)
    (let ([next-ref (length the-store)])
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (let ([val (list-ref the-store ref)])
      (if (undefined-value? val)
          (eopl:error 'deref "Reference is uninitialized.")
          val))))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref
                the-store)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec ([setref-inner (lambda (store1 ref1)
                                   (cond [(null? store1) (report-invalid-reference ref the-store)]
                                         [(zero? ref1) (cons val (cdr store1))]
                                         [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
            (setref-inner the-store ref)))))

;; Interpreter.

(define apply-procedure
  (lambda (proc1 args cont)
    (cases proc proc1
      [procedure (vars body saved-env) (let ([new-env (let loop ([vars vars]
                                                                 [args args]
                                                                 [env saved-env])
                                                        (if (null? vars)
                                                            env
                                                            (loop (cdr vars)
                                                                  (cdr args)
                                                                  (extend-env (car vars)
                                                                              (newref (car args))
                                                                              env))))])
                                         (value-of body new-env cont))])))

(define (apply-cont cont val)
  (cont val))

(define (apply-command-cont cont)
  (cont))

(define value-of
  (lambda (exp env cont)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]
      [var-exp (var) (apply-cont cont (deref (apply-env env var)))]
      [add-exp (exp1 exp2) (value-of exp1
                                     env
                                     (lambda (val1)
                                       (value-of exp2
                                                 env
                                                 (lambda (val2)
                                                   (let ([num1 (expval->num val1)]
                                                         [num2 (expval->num val2)])
                                                     (apply-cont cont (num-val (+ num1 num2))))))))]
      [diff-exp (exp1 exp2) (value-of exp1
                                      env
                                      (lambda (val1)
                                        (value-of exp2
                                                  env
                                                  (lambda (val2)
                                                    (let ([num1 (expval->num val1)]
                                                          [num2 (expval->num val2)])
                                                      (apply-cont cont (num-val (- num1 num2))))))))]
      [multiply-exp (exp1 exp2) (value-of exp1
                                          env
                                          (lambda (val1)
                                            (value-of exp2
                                                      env
                                                      (lambda (val2)
                                                        (let ([num1 (expval->num val1)]
                                                              [num2 (expval->num val2)])
                                                          (apply-cont cont (num-val (* num1 num2))))))))]
      [not-exp (exp) (value-of exp
                               env
                               (lambda (val)
                                 (apply-cont cont (bool-val (not (expval->bool val))))))]
      [zero?-exp (exp1) (value-of exp1
                                  env
                                  (lambda (val)
                                    (apply-cont cont (bool-val (zero? (expval->num val))))))]
      [if-exp (exp1 exp2 exp3) (value-of exp1
                                         env
                                         (lambda (val1)
                                           (if (expval->bool val1)
                                               (value-of exp2 env cont)
                                               (value-of exp3 env cont))))]
      [let-exp (vars exps body) (letrec ([make-cont (lambda (vals exps)
                                                      (if (null? exps)
                                                          (lambda (val)
                                                            (value-of body
                                                                      (let loop ([env env]
                                                                                 [vars vars]
                                                                                 [vals (reverse (cons val vals))])
                                                                        (if (null? vars)
                                                                            env
                                                                            (loop (extend-env (car vars)
                                                                                              (car vals)
                                                                                              env)
                                                                                  (cdr vars)
                                                                                  (cdr vals))))
                                                                      cont))
                                                          (lambda (val)
                                                            (value-of (car exps)
                                                                      env
                                                                      (make-cont (cons val vals)
                                                                                 (cdr exps))))))])
                                  (if (null? vars)
                                      (value-of body env cont)
                                      (value-of (car exps)
                                                env
                                                (make-cont '() (cdr exps)))))]
      [proc-exp (vars body) (apply-cont cont (proc-val (procedure vars body env)))]
      [call-exp (rator rands) (if (null? rands)
                                  (value-of rator
                                            env
                                            (lambda (val1)
                                              (apply-procedure (expval->proc val1) '() cont)))
                                  (value-of rator
                                            env
                                            (lambda (val1)
                                              (letrec ([make-cont (lambda (args rands)
                                                                    (if (null? rands)
                                                                        (lambda (val)
                                                                          (apply-procedure (expval->proc val1)
                                                                                           (reverse (cons val args))
                                                                                           cont))
                                                                        (lambda (val)
                                                                          (value-of (car rands)
                                                                                    env
                                                                                    (make-cont (cons val args)
                                                                                               (cdr rands))))))])
                                                (value-of (car rands)
                                                          env
                                                          (make-cont '() (cdr rands)))))))]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (let* ([refs (map (lambda (p-name)
                                                                            (newref 0))
                                                                          p-names)]
                                                               [rec-env (let loop ([p-names p-names]
                                                                                   [refs refs]
                                                                                   [env env])
                                                                          (if (null? p-names)
                                                                              env
                                                                              (loop (cdr p-names)
                                                                                    (cdr refs)
                                                                                    (extend-env (car p-names)
                                                                                                (car refs)
                                                                                                env))))])
                                                          (for-each (lambda (ref b-vars p-body)
                                                                      (setref! ref
                                                                               (proc-val (procedure b-vars
                                                                                                    p-body
                                                                                                    rec-env))))
                                                                    refs
                                                                    b-vars
                                                                    p-bodies)
                                                          (value-of letrec-body rec-env cont))]
      [begin-exp (exp1 exps) (if (null? exps)
                                 (value-of exp1 env cont)
                                 (letrec ([make-cont (lambda (exps)
                                                       (if (null? exps)
                                                           (lambda (val)
                                                             (apply-cont cont val))
                                                           (lambda (val)
                                                             (value-of (car exps)
                                                                       env
                                                                       (make-cont (cdr exps))))))])
                                   (value-of exp1 env (make-cont exps))))]
      [assign-exp (var exp1) (value-of exp1
                                       env
                                       (lambda (val)
                                         (setref! (apply-env env var) val)
                                         (apply-cont cont (num-val 27))))])))

(define (result-of statement1 env cont)
  (cases statement statement1
    [assign-statement (var exp) (value-of exp
                                          env
                                          (lambda (val)
                                            (setref! (apply-env env var) val)
                                            (apply-command-cont cont)))]
    [print-statement (exp) (value-of exp
                                     env
                                     (lambda (val)
                                       (cases expval val
                                         [num-val (num) (begin (display num)
                                                               (newline))]
                                         [bool-val (bool) (begin (display bool)
                                                                 (newline))]
                                         [proc-val (proc) (begin (display "<procedure>")
                                                                 (newline))])
                                       (apply-command-cont cont)))]
    [brace-statement (statements) (if (null? statements)
                                      (apply-command-cont cont)
                                      (letrec ([make-cont (lambda (statements)
                                                            (if (null? statements)
                                                                (lambda ()
                                                                  (apply-command-cont cont))
                                                                (lambda ()
                                                                  (result-of (car statements)
                                                                             env
                                                                             (make-cont (cdr statements))))))])
                                        (result-of (car statements)
                                                   env
                                                   (make-cont (cdr statements)))))]
    [if-statement (exp statement1 statement2) (value-of exp
                                                        env
                                                        (lambda (val)
                                                          (if (expval->bool val)
                                                              (result-of statement1 env cont)
                                                              (result-of statement2 env cont))))]
    [while-statement (exp statement) (letrec ([statement-cont (lambda ()
                                                                (value-of exp
                                                                          env
                                                                          (lambda (val)
                                                                            (if (expval->bool val)
                                                                                (result-of statement
                                                                                           env
                                                                                           statement-cont)
                                                                                (apply-command-cont cont)))))])
                                       (apply-command-cont statement-cont))]
    [block-statement (vars body) (let ([body-env (let loop ([vars vars]
                                                            [env env])
                                                   (if (null? vars)
                                                       env
                                                       (loop (cdr vars)
                                                             (extend-env (car vars) (newref (undefined-value)) env))))])
                                   (result-of body body-env cont))]))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      [a-program (statement) (result-of statement
                                        (empty-env)
                                        (lambda ()
                                          '()))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide run)
