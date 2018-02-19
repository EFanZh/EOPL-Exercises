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
    [expression ("let2" identifier "=" expression identifier "=" expression "in" expression) let2-exp]
    [expression ("let3" identifier "=" expression identifier "=" expression identifier "=" expression "in" expression)
                let3-exp]
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]
    [expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; Data structures.

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

(define identifier? symbol?)

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont [saved-cont continuation?]]
  [let-exp-cont [var identifier?]
                [body expression?]
                [saved-env environment?]
                [saved-cont continuation?]]
  [let2-exp1-cont [var1 identifier?]
                  [var2 identifier?]
                  [exp2 expression?]
                  [body expression?]
                  [saved-env environment?]
                  [saved-cont continuation?]]
  [let2-exp2-cont [var1 identifier?]
                  [val1 expval?]
                  [var2 identifier?]
                  [body expression?]
                  [saved-env environment?]
                  [saved-cont continuation?]]
  [let3-exp1-cont [var1 identifier?]
                  [var2 identifier?]
                  [exp2 expression?]
                  [var3 identifier?]
                  [exp3 expression?]
                  [body expression?]
                  [saved-env environment?]
                  [saved-cont continuation?]]
  [let3-exp2-cont [var1 identifier?]
                  [val1 expval?]
                  [var2 identifier?]
                  [var3 identifier?]
                  [exp3 expression?]
                  [body expression?]
                  [saved-env environment?]
                  [saved-cont continuation?]]
  [let3-exp3-cont [var1 identifier?]
                  [val1 expval?]
                  [var2 identifier?]
                  [val2 expval?]
                  [var3 identifier?]
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
  [rator-cont [rand expression?]
              [saved-env environment?]
              [saved-cont continuation?]]
  [rand-cont [val1 expval?]
             [saved-cont continuation?]])

;; Interpreter.

(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
      [procedure (var body saved-env) (value-of/k body
                                                  (extend-env var arg saved-env)
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
                                                                (extend-env var val saved-env)
                                                                saved-cont)]
      [let2-exp1-cont (var1 var2 exp2 body saved-env saved-cont) (value-of/k exp2
                                                                             saved-env
                                                                             (let2-exp2-cont var1
                                                                                             val
                                                                                             var2
                                                                                             body
                                                                                             saved-env
                                                                                             saved-cont))]
      [let2-exp2-cont (var1 val1 var2 body saved-env saved-cont) (value-of/k body
                                                                             (extend-env var2
                                                                                         val
                                                                                         (extend-env var1
                                                                                                     val1
                                                                                                     saved-env))
                                                                             saved-cont)]
      [let3-exp1-cont (var1 var2 exp2 var3 exp3 body saved-env saved-cont) (value-of/k exp2
                                                                                       saved-env
                                                                                       (let3-exp2-cont var1
                                                                                                       val
                                                                                                       var2
                                                                                                       var3
                                                                                                       exp3
                                                                                                       body
                                                                                                       saved-env
                                                                                                       saved-cont))]
      [let3-exp2-cont (var1 val1 var2 var3 exp3 body saved-env saved-cont) (value-of/k exp3
                                                                                       saved-env
                                                                                       (let3-exp3-cont var1
                                                                                                       val1
                                                                                                       var2
                                                                                                       val
                                                                                                       var3
                                                                                                       body
                                                                                                       saved-env
                                                                                                       saved-cont))]
      [let3-exp3-cont (var1 val1 var2 val2 var3 body saved-env saved-cont)
                      (value-of/k body
                                  (extend-env var1
                                              val1
                                              (extend-env var2
                                                          val2
                                                          (extend-env var3
                                                                      val
                                                                      saved-env)))
                                  saved-cont)]
      [if-test-cont (exp2 exp3 saved-env saved-cont) (if (expval->bool val)
                                                         (value-of/k exp2 saved-env saved-cont)
                                                         (value-of/k exp3 saved-env saved-cont))]
      [diff1-cont (exp2 saved-env saved-cont) (value-of/k exp2
                                                          saved-env
                                                          (diff2-cont val saved-cont))]
      [diff2-cont (val1 saved-cont) (let ([num1 (expval->num val1)]
                                          [num2 (expval->num val)])
                                      (apply-cont saved-cont (num-val (- num1 num2))))]
      [rator-cont (rand saved-env saved-cont) (value-of/k rand
                                                          saved-env
                                                          (rand-cont val saved-cont))]
      [rand-cont (val1 saved-cont) (let ([proc (expval->proc val1)])
                                     (apply-procedure/k proc val saved-cont))])))

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
      [zero?-exp (exp1) (value-of/k exp1 env (zero1-cont cont))]
      [let-exp (var exp1 body) (value-of/k exp1 env (let-exp-cont var body env cont))]
      [let2-exp (var1 exp1 var2 exp2 body) (value-of/k exp1 env (let2-exp1-cont var1 var2 exp2 body env cont))]
      [let3-exp (var1 exp1 var2 exp2 var3 exp3 body) (value-of/k exp1
                                                                 env
                                                                 (let3-exp1-cont var1
                                                                                 var2
                                                                                 exp2
                                                                                 var3
                                                                                 exp3
                                                                                 body
                                                                                 env
                                                                                 cont))]
      [if-exp (exp1 exp2 exp3) (value-of/k exp1 env (if-test-cont exp2 exp3 env cont))]
      [diff-exp (exp1 exp2) (value-of/k exp1 env (diff1-cont exp2 env cont))]
      [call-exp (rator rand) (value-of/k rator env (rator-cont rand env cont))])))

(define (init-env)
  (empty-env))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of/k exp1
                                    (init-env)
                                    (end-cont))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide bool-val num-val run)
