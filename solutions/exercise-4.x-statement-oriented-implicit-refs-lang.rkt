#lang eopl

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
    [statement ("read" identifier) read-statement]
    [statement ("do" statement "while" expression) do-while-statement]
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
  (lambda (proc1 args)
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
                                         (value-of body new-env))])))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (deref (apply-env env var))]
      [add-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                 [val2 (value-of exp2 env)])
                             (let ([num1 (expval->num val1)]
                                   [num2 (expval->num val2)])
                               (num-val (+ num1 num2))))]
      [diff-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (let ([num1 (expval->num val1)]
                                    [num2 (expval->num val2)])
                                (num-val (- num1 num2))))]
      [multiply-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                      [val2 (value-of exp2 env)])
                                  (let ([num1 (expval->num val1)]
                                        [num2 (expval->num val2)])
                                    (num-val (* num1 num2))))]
      [not-exp (exp) (let ([val (value-of exp env)])
                       (bool-val (not (expval->bool val))))]
      [zero?-exp (exp1) (let ([val1 (value-of exp1 env)])
                          (let ([num1 (expval->num val1)])
                            (if (zero? num1)
                                (bool-val #t)
                                (bool-val #f))))]
      [if-exp (exp1 exp2 exp3) (let ([val1 (value-of exp1 env)])
                                 (if (expval->bool val1)
                                     (value-of exp2 env)
                                     (value-of exp3 env)))]
      [let-exp (vars exps body) (let ([body-env (let loop ([vars vars]
                                                           [exps exps]
                                                           [env1 env])
                                                  (if (null? vars)
                                                      env1
                                                      (loop (cdr vars)
                                                            (cdr exps)
                                                            (extend-env (car vars)
                                                                        (newref (value-of (car exps) env))
                                                                        env1))))])
                                  (value-of body body-env))]
      [proc-exp (vars body) (proc-val (procedure vars body env))]
      [call-exp (rator rands) (let ([proc (expval->proc (value-of rator env))]
                                    [args (map (lambda (rand)
                                                 (value-of rand env))
                                               rands)])
                                (apply-procedure proc args))]
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
                                                          (value-of letrec-body rec-env))]
      [begin-exp (exp1 exps) (letrec ([value-of-begins (lambda (e1 es)
                                                         (let ([v1 (value-of e1 env)])
                                                           (if (null? es)
                                                               v1
                                                               (value-of-begins (car es) (cdr es)))))])
                               (value-of-begins exp1 exps))]
      [assign-exp (var exp1)
                  (setref! (apply-env env var) (value-of exp1 env))
                  (num-val 27)])))

(define (value-of-statement statement1 env)
  (cases statement statement1
    [assign-statement (var exp)
                      (setref! (apply-env env var) (value-of exp env))
                      (num-val 27)]
    [print-statement (exp) (cases expval (value-of exp env)
                             [num-val (num)
                                      (display num)
                                      (newline)]
                             [bool-val (bool)
                                       (display bool)
                                       (newline)]
                             [proc-val (proc)
                                       (display "<procedure>")
                                       (newline)])]
    [brace-statement (statements) (for-each (lambda (s)
                                              (value-of-statement s env))
                                            statements)]
    [if-statement (exp statement1 statement2) (if (expval->bool (value-of exp env))
                                                  (value-of-statement statement1 env)
                                                  (value-of-statement statement2 env))]
    [while-statement (exp statement) (let loop ()
                                       (if (expval->bool (value-of exp env))
                                           (begin (value-of-statement statement env)
                                                  (loop))
                                           'break))]
    [block-statement (vars body) (let ([body-env (let loop ([vars vars]
                                                            [env env])
                                                   (if (null? vars)
                                                       env
                                                       (loop (cdr vars)
                                                             (extend-env (car vars) (newref (undefined-value)) env))))])
                                   (value-of-statement body body-env))]
    [read-statement (var) (let ([num (read)])
                            (if (and (integer? num)
                                     (not (negative? num)))
                                (setref! (apply-env env var) (num-val num))
                                (eopl:error 'value-of-statement "Expect a nonnegative integer, but got ~s." num)))]
    [do-while-statement (statement exp) (let loop ()
                                          (begin (value-of-statement statement env)
                                                 (if (expval->bool (value-of exp env))
                                                     (loop)
                                                     'break)))]))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      [a-program (statement) (value-of-statement statement (empty-env))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide run)
