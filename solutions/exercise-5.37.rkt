#lang eopl

;; Exercise 5.37 [★] Modify the defined language to raise an exception when a procedure is called with the wrong number
;; of arguments.

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
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression) letrec-exp]
    [expression ("list" "(" (separated-list number ",") ")") const-list-exp]
    [expression (unary-op "(" expression ")") unop-exp]
    [expression ("try" expression "catch" "(" identifier ")" expression) try-exp]
    [expression ("raise" expression) raise-exp]
    [unary-op ("null?") null?-unop]
    [unary-op ("car") car-unop]
    [unary-op ("cdr" ) cdr-unop]
    [unary-op ("zero?") zero?-unop]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures.

(define environment?
  (list-of (lambda (p)
             (and (pair? p)
                  (symbol? (car p))))))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extend-env-rec
  (lambda (p-name b-vars p-body saved-env)
    (cons (list p-name b-vars p-body)
          saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ([binding (car env)]
               [id (list-ref binding 0)]
               [expval-or-bvar (list-ref binding 1)])
          (cond [(not (eqv? search-sym id)) (apply-env (cdr env) search-sym)]
                [(expval? expval-or-bvar) expval-or-bvar]
                [else (let ([bvars (cadr binding)]
                            [body (caddr binding)])
                        (proc-val (procedure bvars body env)))])))))

(define-datatype proc proc?
  [procedure [bvars (list-of symbol?)]
             [body expression?]
             [env environment?]])

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]]
  [list-val [lst (list-of expval?)]])

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

(define expval->list
  (lambda (v)
    (cases expval v
      [list-val (lst) lst]
      [else (expval-extractor-error 'list v)])))

(define-datatype continuation continuation?
  [end-cont]
  [diff1-cont [exp2 expression?]
              [env environment?]
              [cont continuation?]]
  [diff2-cont [val1 expval?]
              [cont continuation?]]
  [unop-arg-cont [unop unary-op?]
                 [cont continuation?]]
  [if-test-cont [exp2 expression?]
                [exp3 expression?]
                [env environment?]
                [cont continuation?]]
  [rator-cont [rands (list-of expression?)]
              [env environment?]
              [cont continuation?]]
  [rand-cont [rator proc?]
             [vals (list-of expval?)]
             [rands (list-of expression?)]
             [env environment?]
             [cont continuation?]]
  [try-cont [var symbol?]
            [handler-exp expression?]
            [env environment?]
            [cont continuation?]]
  [raise1-cont [saved-cont continuation?]])

;; Interpreter.

(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
      [null?-unop () (bool-val (null? (expval->list val)))]
      [car-unop () (car (expval->list val))]
      [cdr-unop () (list-val (cdr (expval->list val)))]
      [zero?-unop () (bool-val (zero? (expval->num val)))])))

(define apply-procedure
  (lambda (proc1 args cont)
    (cases proc proc1
      [procedure (vars body saved-env) (value-of/k body
                                                   (let loop ([env saved-env]
                                                              [vars vars]
                                                              [args args])
                                                     (if (null? vars)
                                                         env
                                                         (loop (extend-env (car vars) (car args) env)
                                                               (cdr vars)
                                                               (cdr args))))
                                                   cont)])))

(define apply-handler
  (lambda (val cont)
    (cases continuation cont
      [try-cont (var handler-exp saved-env saved-cont) (value-of/k handler-exp
                                                                   (extend-env var val saved-env)
                                                                   saved-cont)]
      [end-cont () (eopl:error 'apply-handler "uncaught exception!")]
      [diff1-cont (exp2 saved-env saved-cont) (apply-handler val saved-cont)]
      [diff2-cont (val1 saved-cont) (apply-handler val saved-cont)]
      [if-test-cont (exp2 exp3 env saved-cont) (apply-handler val saved-cont)]
      [unop-arg-cont (unop saved-cont) (apply-handler val saved-cont)]
      [rator-cont (rands saved-env saved-cont) (apply-handler val saved-cont)]
      [rand-cont (rator vals rands saved-env saved-cont) (apply-handler val saved-cont)]
      [raise1-cont (cont) (apply-handler val cont)])))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [end-cont () val]
      [diff1-cont (exp2 saved-env saved-cont) (value-of/k exp2 saved-env (diff2-cont val saved-cont))]
      [diff2-cont (val1 saved-cont) (let ([n1 (expval->num val1)]
                                          [n2 (expval->num val)])
                                      (apply-cont saved-cont (num-val (- n1 n2))))]
      [unop-arg-cont (unop cont) (apply-cont cont (apply-unop unop val))]
      [if-test-cont (exp2 exp3 env cont) (if (expval->bool val)
                                             (value-of/k exp2 env cont)
                                             (value-of/k exp3 env cont))]
      [rator-cont (rands saved-env saved-cont)
                  (let ([proc1 (expval->proc val)])
                    (cases proc proc1
                      [procedure (bvars body env)
                                 (if (= (length bvars) (length rands))
                                     (if (null? rands)
                                         (apply-procedure proc1 '() saved-cont)
                                         (value-of/k (car rands)
                                                     saved-env
                                                     (rand-cont proc1
                                                                '()
                                                                (cdr rands)
                                                                saved-env
                                                                saved-cont)))
                                     (apply-handler (num-val 777) cont))]))]
      [rand-cont (rator vals rands saved-env saved-cont) (if (null? rands)
                                                             (apply-procedure rator
                                                                              (reverse (cons val vals))
                                                                              saved-cont)
                                                             (value-of/k (car rands)
                                                                         saved-env
                                                                         (rand-cont rator
                                                                                    (cons val vals)
                                                                                    (cdr rands)
                                                                                    saved-env
                                                                                    saved-cont)))]
      [try-cont (var handler-exp saved-env saved-cont) (apply-cont saved-cont val)]
      [raise1-cont (saved-cont) (apply-handler val saved-cont)])))

(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]
      [const-list-exp (nums) (apply-cont cont (list-val (map num-val nums)))]
      [var-exp (var) (apply-cont cont (apply-env env var))]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (diff1-cont exp2 env cont))]
      [unop-exp (unop exp1) (value-of/k exp1 env (unop-arg-cont unop cont))]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (if-test-cont exp2 exp3 env cont))]
      [proc-exp (vars body) (apply-cont cont (proc-val (procedure vars body env)))]
      [call-exp (rator rands) (value-of/k rator env (rator-cont rands env cont))]
      [let-exp (var exp1 body) (value-of/k (call-exp (proc-exp (list var) body) (list exp1)) env cont)]
      [letrec-exp (p-name b-vars p-body letrec-body) (value-of/k letrec-body
                                                                 (extend-env-rec p-name b-vars p-body env)
                                                                 cont)]
      [try-exp (exp1 var handler-exp) (value-of/k exp1 env (try-cont var handler-exp env cont))]
      [raise-exp (exp1) (value-of/k exp1 env (raise1-cont cont))])))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (value-of/k body (empty-env) (end-cont))))))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val list-val num-val run)
