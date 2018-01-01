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

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures.

(define-datatype proc proc?
  [procedure [bvar symbol?]
             [body expression?]
             [env environment?]])

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]]
  [ref-val [ref reference?]])

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

(define expval->ref
  (lambda (v)
    (cases expval v
      [ref-val (ref) ref]
      [else (expval-extractor-error 'reference v)])))

;; Environments.

(define reference?
  (lambda (v)
    (integer? v)))

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval reference?]
              [saved-env environment?]]
  [extend-env-rec* [proc-names (list-of symbol?)]
                   [b-vars (list-of symbol?)]
                   [proc-bodies (list-of expression?)]
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
                                            (apply-env saved-env search-var))]
      [extend-env-rec* (p-names b-vars p-bodies saved-env) (let ([n (location search-var p-names)])
                                                             (if n
                                                                 (newref (proc-val (procedure  (list-ref b-vars n)
                                                                                               (list-ref p-bodies n)
                                                                                               env)))
                                                                 (apply-env saved-env search-var)))])))

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
  (lambda (proc1 arg)
    (cases proc proc1
      [procedure (var body saved-env) (let ([r (newref arg)])
                                        (let ([new-env (extend-env var r saved-env)])
                                          (value-of body new-env)))])))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (deref (apply-env env var))]
      [diff-exp (exp1 exp2) (let ([val1 (value-of exp1 env)]
                                  [val2 (value-of exp2 env)])
                              (let ([num1 (expval->num val1)]
                                    [num2 (expval->num val2)])
                                (num-val (- num1 num2))))]
      [zero?-exp (exp1) (let ([val1 (value-of exp1 env)])
                          (let ([num1 (expval->num val1)])
                            (if (zero? num1)
                                (bool-val #t)
                                (bool-val #f))))]
      [if-exp (exp1 exp2 exp3) (let ([val1 (value-of exp1 env)])
                                 (if (expval->bool val1)
                                     (value-of exp2 env)
                                     (value-of exp3 env)))]
      [let-exp (var exp1 body) (let ([v1 (value-of exp1 env)])
                                 (value-of body
                                           (extend-env var (newref v1) env)))]        
      [proc-exp (var body) (proc-val (procedure var body env))]
      [call-exp (rator rand) (let ([proc (expval->proc (value-of rator env))]
                                   [arg (value-of rand env)])
                               (apply-procedure proc arg))]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (value-of letrec-body
                                                                  (extend-env-rec* p-names b-vars p-bodies env))]
      [begin-exp (exp1 exps) (letrec ([value-of-begins (lambda (e1 es)
                                                         (let ([v1 (value-of e1 env)])
                                                           (if (null? es)
                                                               v1
                                                               (value-of-begins (car es) (cdr es)))))])
                               (value-of-begins exp1 exps))]
      [assign-exp (var exp1) (begin (setref! (apply-env env var)
                                             (value-of exp1 env))
                                    (num-val 27))])))

(define value-of-program 
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (empty-env))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide num-val bool-val run)
