#lang eopl

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
    [expression ("*" "(" expression "," expression ")") multiply-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    [expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression) letrec-exp]
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("set" identifier "=" expression) assign-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures.

(define identifier? symbol?)
(define reference? integer?)

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

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval reference?]
              [saved-env environment?]]
  [extend-env-rec* [proc-names (list-of symbol?)]
                   [b-vars (list-of symbol?)]
                   [proc-bodies (list-of expression?)]
                   [saved-env environment?]])

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont [saved-cont continuation?]]
  [let-exp-cont [var identifier?]
                [body expression?]
                [saved-env environment?]
                [saved-cont continuation?]]
  [if-test-cont [exp2 expression?]
                [exp3 expression?]
                [saved-env environment?]
                [saved-cont continuation?]]
  [diff1-cont [exp2 expression?]
              [saved-env environment?]
              [saved-cont continuation?]]
  [diff2-cont [val1 expval?]
              [saved-cont continuation?]]
  [multiply1-cont [exp2 expression?]
                  [saved-env environment?]
                  [saved-cont continuation?]]
  [multiply2-cont [val1 expval?]
                  [saved-cont continuation?]]
  [rator-cont [rand expression?]
              [saved-env environment?]
              [saved-cont continuation?]]
  [rand-cont [val1 expval?]
             [saved-cont continuation?]]
  [begin-cont [exps (list-of expression?)]
              [saved-env environment?]
              [saved-cont continuation?]]
  [set-rhs-cont [ref reference?]
                [saved-cont continuation?]])

;; Store.

(define the-store 'uninitialized)

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
    (list-ref the-store ref)))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref "illegal reference ~s in store ~s" ref the-store)))

(define setref!
  (lambda (ref val)
    (set! the-store (letrec ([setref-inner (lambda (store1 ref1)
                                             (cond [(null? store1) (report-invalid-reference ref the-store)]
                                                   [(zero? ref1) (cons val (cdr store1))]
                                                   [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
                      (setref-inner the-store ref)))))

;; Interpreter.

(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
      [procedure (var body saved-env) (value-of/k body
                                                  (extend-env var (newref arg) saved-env)
                                                  cont)])))

(define used-end-conts '())

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [end-cont () (if (memq cont used-end-conts)
                       (eopl:error "Continuation is already used.")
                       (begin (set! used-end-conts (cons cont used-end-conts))
                              val))]
      [zero1-cont (saved-cont) (apply-cont saved-cont (bool-val (zero? (expval->num val))))]
      [let-exp-cont (var body saved-env saved-cont) (value-of/k body
                                                                (extend-env var (newref val) saved-env)
                                                                saved-cont)]
      [if-test-cont (exp2 exp3 saved-env saved-cont) (if (expval->bool val)
                                                         (value-of/k exp2 saved-env saved-cont)
                                                         (value-of/k exp3 saved-env saved-cont))]
      [diff1-cont (exp2 saved-env saved-cont) (value-of/k exp2 saved-env (diff2-cont val saved-cont))]
      [diff2-cont (val1 saved-cont) (let ([num1 (expval->num val1)]
                                          [num2 (expval->num val)])
                                      (apply-cont saved-cont (num-val (- num1 num2))))]
      [multiply1-cont (exp2 saved-env saved-cont) (value-of/k exp2 saved-env (multiply2-cont val saved-cont))]
      [multiply2-cont (val1 saved-cont) (let ([num1 (expval->num val1)]
                                              [num2 (expval->num val)])
                                          (apply-cont saved-cont (num-val (* num1 num2))))]
      [rator-cont (rand saved-env saved-cont) (value-of/k rand saved-env (rand-cont val saved-cont))]
      [rand-cont (val1 saved-cont) (let ([proc (expval->proc val1)])
                                     (apply-procedure/k proc val saved-cont))]
      [begin-cont (exps saved-env saved-cont) (if (null? exps)
                                                  (apply-cont saved-cont val)
                                                  (value-of/k (car exps)
                                                              saved-env
                                                              (begin-cont (cdr exps)
                                                                          saved-env
                                                                          saved-cont)))]
      [set-rhs-cont (ref saved-cont) (begin (setref! ref val)
                                            (apply-cont saved-cont (num-val 27)))])))

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
                                            (apply-env saved-env search-var))]
      [extend-env-rec* (p-names b-vars p-bodies saved-env) (let ([n (location search-var p-names)])
                                                             (if n
                                                                 (newref (proc-val (procedure (list-ref b-vars n)
                                                                                              (list-ref p-bodies n)
                                                                                              env)))
                                                                 (apply-env saved-env search-var)))])))

(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]
      [var-exp (var) (apply-cont cont (deref (apply-env env var)))]
      [proc-exp (var body) (apply-cont cont (proc-val (procedure var body env)))]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (value-of/k letrec-body
                                                                    (extend-env-rec* p-names b-vars p-bodies env)
                                                                    cont)]
      [zero?-exp (exp1) (value-of/k exp1 env (zero1-cont cont))]
      [let-exp (var exp1 body) (value-of/k exp1 env (let-exp-cont var body env cont))]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (if-test-cont exp2 exp3 env cont))]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (diff1-cont exp2 env cont))]
      [multiply-exp (exp1 exp2) (value-of/k exp1 env (multiply1-cont exp2 env cont))]
      [call-exp (rator rand) (value-of/k rator env (rator-cont rand env cont))]
      [begin-exp (exp1 exps) (value-of/k exp1 env (begin-cont exps env cont))]
      [assign-exp (var exp1) (value-of/k exp1 env (set-rhs-cont (apply-env env var) cont))])))

(define (init-env)
  (empty-env))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      [a-program (exp1) (value-of/k exp1
                                    (init-env)
                                    (end-cont))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val num-val run)
