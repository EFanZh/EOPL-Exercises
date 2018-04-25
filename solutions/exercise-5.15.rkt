#lang eopl

;; Exercise 5.15 [★] Our continuation data type contains just the single constant, end-cont, and all the other
;; continuation-builders have a single continuation argument. Implement continuations by representing them as lists,
;; where (end-cont) is represented by the empty list, and each other continuation is represented by a nonempty list
;; whose car contains a distinctive data structure (called frame or activation record) and whose cdr contains the
;; embedded continuation. Observe that the interpreter treats these lists like a stack (of frames).

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
    [expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures.

(define identifier? symbol?)

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
              [bval expval?]
              [saved-env environment?]]
  [extend-env-rec [p-name symbol?]
                  [b-var symbol?]
                  [p-body expression?]
                  [saved-env environment?]])

(define (end-cont)
  '())

(define-datatype frame frame?
  [zero1-frame]
  [let-exp-frame [var identifier?]
                 [body expression?]
                 [saved-env environment?]]
  [if-test-frame [exp2 expression?]
                 [exp3 expression?]
                 [saved-env environment?]]
  [diff1-frame [exp2 expression?]
               [saved-env environment?]]
  [diff2-frame [val1 expval?]]
  [rator-frame [rand expression?]
               [saved-env environment?]]
  [rand-frame [val1 expval?]])

;; Interpreter.

(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
      [procedure (var body saved-env) (value-of/k body
                                                  (extend-env var arg saved-env)
                                                  cont)])))

(define apply-cont
  (lambda (cont val)
    (if (null? cont)
        val
        (let ([current-frame (car cont)]
              [saved-cont (cdr cont)])
          (cases frame current-frame
            [zero1-frame () (apply-cont saved-cont (bool-val (zero? (expval->num val))))]
            [let-exp-frame (var body saved-env) (value-of/k body
                                                            (extend-env var val saved-env)
                                                            saved-cont)]
            [if-test-frame (exp2 exp3 saved-env) (if (expval->bool val)
                                                     (value-of/k exp2 saved-env saved-cont)
                                                     (value-of/k exp3 saved-env saved-cont))]
            [diff1-frame (exp2 saved-env) (value-of/k exp2 saved-env (cons (diff2-frame val) saved-cont))]
            [diff2-frame (val1) (let ([num1 (expval->num val1)]
                                      [num2 (expval->num val)])
                                  (apply-cont saved-cont (num-val (- num1 num2))))]
            [rator-frame (rand saved-env) (value-of/k rand saved-env (cons (rand-frame val) saved-cont))]
            [rand-frame (val1) (let ([proc (expval->proc val1)])
                                 (apply-procedure/k proc val saved-cont))])))))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (var val saved-env) (if (eqv? search-sym var)
                                          val
                                          (apply-env saved-env search-sym))]
      [extend-env-rec (p-name b-var p-body saved-env) (if (eqv? search-sym p-name)
                                                          (proc-val (procedure b-var p-body env))
                                                          (apply-env saved-env search-sym))])))

(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      [const-exp (num) (apply-cont cont (num-val num))]
      [var-exp (var) (apply-cont cont (apply-env env var))]
      [proc-exp (var body) (apply-cont cont (proc-val (procedure var body env)))]
      [letrec-exp (p-name b-var p-body letrec-body) (value-of/k letrec-body
                                                                (extend-env-rec p-name b-var p-body env)
                                                                cont)]
      [zero?-exp (exp1) (value-of/k exp1 env (cons (zero1-frame) cont))]
      [let-exp (var exp1 body) (value-of/k exp1 env (cons (let-exp-frame var body env) cont))]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (cons (if-test-frame exp2 exp3 env) cont))]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (cons (diff1-frame exp2 env) cont))]
      [call-exp (rator rand) (value-of/k rator env (cons (rator-frame rand env) cont))])))

(define (init-env)
  (empty-env))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of/k exp1 (init-env) (end-cont))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val num-val run)
