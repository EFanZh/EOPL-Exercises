#lang eopl

;; Exercise 6.8 [★★★] Rewrite the interpreter of section 5.4 using a procedural and inlined representation. This is
;; challenging because we effectively have two observers, apply-cont and apply-handler. As a hint, consider modifying
;; the recipe on page 6.1 so that we add to each procedure two extra arguments, one representing the behavior of the
;; continuation under apply-cont and one representing its behavior under apply-handler.

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
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp]
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
  (lambda (p-name b-var p-body saved-env)
    (cons (list p-name b-var p-body)
          saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ([binding (car env)]
               [id (list-ref binding 0)]
               [expval-or-bvar (list-ref binding 1)])
          (cond [(not (eqv? search-sym id)) (apply-env (cdr env) search-sym)]
                [(not (symbol? expval-or-bvar)) expval-or-bvar]
                [else (let ([bvar (cadr binding)]
                            [body (caddr binding)])
                        (proc-val (procedure bvar body env)))])))))

(define-datatype proc proc?
  [procedure [bvar symbol?]
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

;; Continuations.

(define end-cont
  (lambda ()
    (lambda (val)
      val)))

(define diff1-cont
  (lambda (exp2 env cont handler)
    (lambda (val)
      (value-of/k exp2 env (diff2-cont val cont) handler))))

(define diff2-cont
  (lambda (val1 cont)
    (lambda (val)
      (let ([n1 (expval->num val1)]
            [n2 (expval->num val)])
        (apply-cont cont (num-val (- n1 n2)))))))

(define unop-arg-cont
  (lambda (unop cont)
    (lambda (val)
      (apply-cont cont (apply-unop unop val)))))

(define if-test-cont
  (lambda (exp2 exp3 env cont handler)
    (lambda (val)
      (if (expval->bool val)
          (value-of/k exp2 env cont handler)
          (value-of/k exp3 env cont handler)))))

(define rator-cont
  (lambda (rand env cont handler)
    (lambda (val)
      (value-of/k rand env (rand-cont val cont handler) handler))))

(define rand-cont
  (lambda (val1 cont handler)
    (lambda (val)
      (let ([proc (expval->proc val1)])
        (apply-procedure proc val cont handler)))))

(define raise-cont
  (lambda (handler)
    (lambda (val)
      (apply-handler handler val))))

(define end-try-handler
  (lambda ()
    (lambda (val)
      (eopl:error 'apply-handler "uncaught exception!"))))

(define try-handler
  (lambda (var handler-exp env cont handler)
    (lambda (val)
      (value-of/k handler-exp
                  (extend-env var val env)
                  cont
                  handler))))

;; Interpreter.

(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
      [null?-unop () (bool-val (null? (expval->list val)))]
      [car-unop () (car (expval->list val))]
      [cdr-unop () (list-val (cdr (expval->list val)))]
      [zero?-unop () (bool-val (zero? (expval->num val)))])))

(define apply-procedure
  (lambda (proc1 arg cont handler)
    (cases proc proc1
      [procedure (var body saved-env) (value-of/k body (extend-env var arg saved-env) cont handler)])))

(define apply-handler
  (lambda (handler val)
    (handler val)))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define value-of/k
  (lambda (exp env cont handler)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]
      [const-list-exp (nums) (apply-cont cont (list-val (map num-val nums)))]
      [var-exp (var) (apply-cont cont (apply-env env var))]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (diff1-cont exp2 env cont handler) handler)]
      [unop-exp (unop exp1) (value-of/k exp1 env (unop-arg-cont unop cont) handler)]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (if-test-cont exp2 exp3 env cont handler) handler)]
      [proc-exp (var body) (apply-cont cont (proc-val (procedure var body env)))]
      [call-exp (rator rand) (value-of/k rator env (rator-cont rand env cont handler) handler)]
      [let-exp (var exp1 body) (value-of/k (call-exp (proc-exp var body) exp1) env cont handler)]
      [letrec-exp (p-name b-var p-body letrec-body) (value-of/k letrec-body
                                                                (extend-env-rec p-name b-var p-body env)
                                                                cont
                                                                handler)]
      [try-exp (exp1 var handler-exp) (value-of/k exp1 env cont (try-handler var handler-exp env cont handler))]
      [raise-exp (exp1) (value-of/k exp1 env (raise-cont handler) handler)])))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (value-of/k body (empty-env) (end-cont) (end-try-handler))))))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val list-val num-val run)
