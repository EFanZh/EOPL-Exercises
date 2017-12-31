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
    [expression ("emptylist") emptylist-exp]
    [expression ("null?" "(" expression ")") null?-exp]
    [expression ("cons" "(" expression "," expression ")") cons-exp]
    [expression ("car" "(" expression ")") car-exp]
    [expression ("cdr" "(" expression ")") cdr-exp]
    [expression ("list" "(" (separated-list expression ",") ")") list-exp]
    [expression ("newref" "(" expression ")") newref-exp]
    [expression ("deref" "(" expression ")") deref-exp]
    [expression ("setref" "(" expression "," expression ")") setref-exp]))

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
  [emptylist-val]
  [pair-val [car expval?]
            [cdr expval?]]
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

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval expval?]
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
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (bvar bval saved-env) (if (eqv? search-sym bvar)
                                            bval
                                            (apply-env saved-env search-sym))]
      [extend-env-rec* (p-names b-vars p-bodies saved-env)
                       (cond [(location search-sym p-names) => (lambda (n)
                                                                 (proc-val (procedure (list-ref b-vars n)
                                                                                      (list-ref p-bodies n)
                                                                                      env)))]
                             [else (apply-env saved-env search-sym)])])))

;; Store.

(define reference?
  (lambda (v)
    (integer? v)))

(define store? (list-of expval?))

(define empty-store
  (lambda ()
    '()))

(define (newref store val)
  (let ([next-ref (length store)]
        [new-store (append store (list val))])
    (cons next-ref new-store)))

(define (deref store ref)
  (list-ref store ref))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref
                the-store)))

(define (setref! store ref val)
  (let loop ([store1 store]
             [ref1 ref])
    (cond [(null? store1) (report-invalid-reference ref store)]
          [(zero? ref1) (cons val (cdr store1))]
          [else (cons (car store1)
                      (loop (cdr store1) (- ref1 1)))])))

;; Interpreter.

(define-datatype answer answer?
  [an-answer [val expval?]
             [store store?]])

(define (apply-procedure proc1 arg store)
  (cases proc proc1
    [procedure (bvar body saved-env) (value-of body
                                               (extend-env bvar arg saved-env)
                                               store)]))

(define value-of
  (lambda (exp env store)
    (cases expression exp
      [const-exp (num) (an-answer (num-val num) store)]
      [var-exp (var) (an-answer (apply-env env var) store)]
      [diff-exp (exp1 exp2) (cases answer (value-of exp1 env store)
                              [an-answer (val1 store1) (cases answer (value-of exp2 env store1)
                                                         [an-answer (val2 store2) (let ([num1 (expval->num val1)]
                                                                                        [num2 (expval->num val2)])
                                                                                    (an-answer (num-val (- num1 num2))
                                                                                               store2))])])]
      [zero?-exp (exp) (cases answer (value-of exp env store)
                         [an-answer (val1 store1) (let ([num1 (expval->num val1)])
                                                    (if (zero? num1)
                                                        (an-answer (bool-val #t) store1)
                                                        (an-answer (bool-val #f) store1)))])]
      [if-exp (exp1 exp2 exp3) (cases answer (value-of exp1 env store)
                                 [an-answer (val store1) (value-of (if (expval->bool val)
                                                                       exp2
                                                                       exp3)
                                                                   env
                                                                   store1)])]
      [let-exp (var exp1 body) (cases answer (value-of exp1 env store)
                                 [an-answer (val1 store1) (value-of body (extend-env var val1 env) store1)])]
      [proc-exp (var body) (an-answer (proc-val (procedure var body env)) store)]
      [call-exp (rator rand) (cases answer (value-of rator env store)
                               [an-answer (proc store1) (cases answer (value-of rand env store1)
                                                          [an-answer (arg store2) (apply-procedure (expval->proc proc)
                                                                                                   arg
                                                                                                   store2)])])]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (value-of letrec-body
                                                                  (extend-env-rec* p-names
                                                                                   b-vars
                                                                                   p-bodies
                                                                                   env)
                                                                  store)]
      [begin-exp (exp1 exps) (let loop ([e1 exp1]
                                        [es exps]
                                        [store store])
                               (let ([current-answer (value-of e1 env store)])
                                 (if (null? es)
                                     current-answer
                                     (cases answer current-answer
                                       [an-answer (val store1)
                                                  (loop (car es) (cdr es) store1)]))))]
      [emptylist-exp () (an-answer (emptylist-val) store)]
      [null?-exp (exp) (cases answer (value-of exp env store)
                         [an-answer (val store1) (cases expval val
                                                   [emptylist-val () (an-answer (bool-val #t) store1)]
                                                   [else (an-answer (bool-val #f) store1)])])]
      [cons-exp (exp1 exp2) (cases answer (value-of exp1 env store)
                              [an-answer (val1 store1) (cases answer (value-of exp2 env store1)
                                                         [an-answer (val2 store2) (an-answer (pair-val val1 val2)
                                                                                             store2)])])]
      [car-exp (exp) (cases answer (value-of exp env store)
                       [an-answer (val store1) (cases expval val
                                                 [pair-val (car cdr) (an-answer car store1)]
                                                 [else (eopl:error 'value-of "Expect a pair, but got ~s." val)])])]
      [cdr-exp (exp) (cases answer (value-of exp env store)
                       [an-answer (val store1) (cases expval val
                                                 [pair-val (car cdr) (an-answer cdr store1)]
                                                 [else (eopl:error 'value-of "Expect a pair, but got ~s." val)])])]
      [list-exp (exps) (let loop ([exps exps]
                                  [store store])
                         (if (null? exps)
                             (an-answer (emptylist-val) store)
                             (cases answer (value-of (car exps) env store)
                               [an-answer (val1 store1) (cases answer (loop (cdr exps) store1)
                                                          [an-answer (val2 store2) (an-answer (pair-val val1 val2)
                                                                                              store2)])])))]
      [newref-exp (exp) (cases answer (value-of exp env store)
                          [an-answer (val store1) (let* ([ref-and-store (newref store1 val)]
                                                         [ref (car ref-and-store)]
                                                         [store1 (cdr ref-and-store)])
                                                    (an-answer (ref-val ref) store1))])]
      [deref-exp (exp) (cases answer (value-of exp env store)
                         [an-answer (val store1) (let ([ref (expval->ref val)])
                                                   (an-answer (deref store1 ref) store1))])]
      [setref-exp (exp1 exp2) (cases answer (value-of exp1 env store)
                                [an-answer (val1 store1) (cases answer (value-of exp2 env store1)
                                                           [an-answer (val2 store2) (let* ([ref (expval->ref val1)]
                                                                                           [store2 (setref! store2
                                                                                                            ref
                                                                                                            val2)])
                                                                                      (an-answer (num-val 23)
                                                                                                 store2))])])])))

;; Interface.

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (empty-env) (empty-store))])))

(define run
  (lambda (string)
    (cases answer (value-of-program (scan&parse string))
      [an-answer (val store) val])))

(provide num-val bool-val emptylist-val pair-val run)
