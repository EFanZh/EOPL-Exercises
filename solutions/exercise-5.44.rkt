#lang eopl

;; Exercise 5.44 [★★] An alternative to letcc and throw of the preceding exercises is to add a single procedure to the
;; language. This procedure, which in Scheme is called call-with-current-continuation, takes a one-argument procedure,
;; p, and passes to p a procedure that when invoked with one argument, passes that argument to the current continuation,
;; cont. We could define call-with-current-continuation in terms of letcc and throw as follows:
;;
;;     let call-with-current-continuation
;;           = proc (p)
;;               letcc cont
;;               in (p proc (v) throw v to cont)
;;     in ...
;;
;; Add call-with-current-continuation to the language. Then write a translator that takes the language with letcc and
;; throw and translates it into the language without letcc and throw, but with call-with-current-continuation.

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
    [expression ("callcc" "(" expression ")") callcc-exp]
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
  [list-val [lst (list-of expval?)]]
  [cont-val [cont continuation?]])

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

(define expval->cont
  (lambda (v)
    (cases expval v
      [cont-val (cont) cont]
      [else (expval-extractor-error 'cont v)])))

(define-datatype continuation continuation?
  [end-cont]
  [diff1-cont [exp2 expression?]
              [env environment?]
              [cont continuation?]
              [saved-try-cont continuation?]]
  [diff2-cont [val1 expval?]
              [cont continuation?]]
  [unop-arg-cont [unop unary-op?]
                 [cont continuation?]]
  [if-test-cont [exp2 expression?]
                [exp3 expression?]
                [env environment?]
                [cont continuation?]
                [saved-try-cont continuation?]]
  [rator-cont [rand expression?]
              [env environment?]
              [cont continuation?]
              [saved-try-cont continuation?]]
  [rand-cont [val1 expval?]
             [cont continuation?]
             [saved-try-cont continuation?]]
  [try-cont [var symbol?]
            [handler-exp expression?]
            [env environment?]
            [cont continuation?]
            [saved-try-cont continuation?]]
  [raise1-cont [saved-cont continuation?]]
  [callcc-cont [cont continuation?]
               [saved-try-cont continuation?]])

;; Interpreter.

(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
      [null?-unop () (bool-val (null? (expval->list val)))]
      [car-unop () (car (expval->list val))]
      [cdr-unop () (list-val (cdr (expval->list val)))]
      [zero?-unop () (bool-val (zero? (expval->num val)))])))

(define apply-procedure
  (lambda (proc1 arg cont try-cont1)
    (cases proc proc1
      [procedure (var body saved-env) (value-of/k body (extend-env var arg saved-env) cont try-cont1)])))

(define apply-handler
  (lambda (val cont)
    (cases continuation cont
      [try-cont (var handler-exp saved-env saved-cont saved-try-cont) (value-of/k handler-exp
                                                                                  (extend-env var val saved-env)
                                                                                  saved-cont
                                                                                  saved-try-cont)]
      [end-cont () (eopl:error 'apply-handler "uncaught exception!")]
      [else (eopl:error 'apply-handler "not a try-cont!")])))

(define (apply-call rator rand cont try-cont1)
  (cases expval rator
    [proc-val (proc) (apply-procedure proc rand cont try-cont1)]
    [cont-val (cont) (apply-cont cont rand)]
    [else (eopl:error 'apply-cont "expect a procedure or a continuation")]))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [end-cont () val]
      [diff1-cont (exp2 saved-env saved-cont saved-try-cont) (value-of/k exp2
                                                                         saved-env
                                                                         (diff2-cont val saved-cont)
                                                                         saved-try-cont)]
      [diff2-cont (val1 saved-cont) (let ([n1 (expval->num val1)]
                                          [n2 (expval->num val)])
                                      (apply-cont saved-cont (num-val (- n1 n2))))]
      [unop-arg-cont (unop cont) (apply-cont cont (apply-unop unop val))]
      [if-test-cont (exp2 exp3 env cont saved-try-cont) (if (expval->bool val)
                                                            (value-of/k exp2 env cont saved-try-cont)
                                                            (value-of/k exp3 env cont saved-try-cont))]
      [rator-cont (rand saved-env saved-cont saved-try-cont) (value-of/k rand
                                                                         saved-env
                                                                         (rand-cont val saved-cont saved-try-cont)
                                                                         saved-try-cont)]
      [rand-cont (val1 saved-cont saved-try-cont) (apply-call val1 val saved-cont saved-try-cont)]
      [try-cont (var handler-exp saved-env saved-cont saved-try-cont) (apply-cont saved-cont val)]
      [raise1-cont (saved-cont) (apply-handler val saved-cont)]
      [callcc-cont (saved-cont saved-try-cont) (apply-call val (cont-val saved-cont) saved-cont saved-try-cont)])))

(define value-of/k
  (lambda (exp env cont try-cont1)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]
      [const-list-exp (nums) (apply-cont cont (list-val (map num-val nums)))]
      [var-exp (var) (apply-cont cont (apply-env env var))]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (diff1-cont exp2 env cont try-cont1) try-cont1)]
      [unop-exp (unop exp1) (value-of/k exp1 env (unop-arg-cont unop cont) try-cont1)]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (if-test-cont exp2 exp3 env cont try-cont1) try-cont1)]
      [proc-exp (var body) (apply-cont cont (proc-val (procedure var body env)))]
      [call-exp (rator rand) (value-of/k rator env (rator-cont rand env cont try-cont1) try-cont1)]
      [let-exp (var exp1 body) (value-of/k (call-exp (proc-exp var body) exp1) env cont try-cont1)]
      [letrec-exp (p-name b-var p-body letrec-body) (value-of/k letrec-body
                                                                (extend-env-rec p-name b-var p-body env)
                                                                cont
                                                                try-cont1)]
      [try-exp (exp1 var handler-exp) (let ([try-cont2 (try-cont var handler-exp env cont try-cont1)])
                                        (value-of/k exp1 env try-cont2 try-cont2))]
      [raise-exp (exp1) (value-of/k exp1 env (raise1-cont try-cont1) try-cont1)]
      [callcc-exp (exp1) (value-of/k exp1 env (callcc-cont cont try-cont1) try-cont1)])))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (value-of/k body (empty-env) (end-cont) (end-cont))))))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val list-val num-val run)
