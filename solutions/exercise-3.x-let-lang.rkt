#lang eopl

;; This code is a implementation of an extended version of LET language.

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [emptylist-val]
  [pair-val [car expval?]
            [cdr expval?]])

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

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors
                "Looking for a ~s, found ~s"
                variant
                value)))

(define empty-env-record
  (lambda ()
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define init-env empty-env)

(define empty-env?
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ([sym (extended-env-record->sym env)]
              [val (extended-env-record->val env)]
              [old-env (extended-env-record->old-env env)])
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [unary-operator ((or "car" "cdr" "minus" "print")) string] ; Use a symbol output leads to error, don’t know why.
    [binary-operator ((or "-" "*" "/" "+" "cons")) string]
    [n-ary-operator ("list") string]
    [bool-unary-operator ((or "null?" "zero?")) string]
    [bool-binary-operator ((or "equal?" "greater?" "less?")) string]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression ("if" bool-exp "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    [expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp]
    [expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp]
    [expression ("emptylist") emptylist-exp]
    [expression ("cond" (arbno bool-exp "==>" expression) "end") cond-exp]
    [expression (unary-operator "(" expression ")") unary-app-exp]
    [expression (binary-operator "(" expression "," expression ")") binary-app-exp]
    [expression (n-ary-operator "(" (separated-list expression ",") ")") n-ary-app-exp]
    [expression (bool-exp) a-bool-exp]
    [bool-exp (bool-unary-operator "(" expression ")") bool-unary-app-exp]
    [bool-exp (bool-binary-operator "(" expression "," expression ")") bool-binary-app-exp]))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env env var)]
      [if-exp (exp1 exp2 exp3) (let* ([val1 (value-of-bool-exp exp1 env)])
                                 (if (expval->bool val1)
                                     (value-of exp2 env)
                                     (value-of exp3 env)))]
      [let-exp (vars exps body) (let ([vals (map (lambda (e)
                                                   (value-of e env))
                                                 exps)])
                                  (value-of body
                                            (let loop ([env env]
                                                       [vars vars]
                                                       [vals vals])
                                              (if (null? vars)
                                                  env
                                                  (loop (extend-env (car vars) (car vals) env)
                                                        (cdr vars)
                                                        (cdr vals))))))]
      [let*-exp (vars exps body) (let loop ([env env]
                                            [vars vars]
                                            [exps exps])
                                   (if (null? vars)
                                       (value-of body env)
                                       (loop (extend-env (car vars) (value-of (car exps) env) env)
                                             (cdr vars)
                                             (cdr exps))))]
      [unpack-exp (vars exp1 body) (let loop ([env env]
                                              [vars vars]
                                              [vals (value-of exp1 env)])
                                     (if (null? vars)
                                         (cases expval vals
                                           [emptylist-val () (value-of body env)]
                                           [pair-val (car-vals cdr-vals) (eopl:error 'value-of
                                                                                     "Too many values to unpack.")]
                                           [else (eopl:error 'value-of "Expect a pair.")])
                                         (cases expval vals
                                           [emptylist-val () (eopl:error 'value-of "Not enough values to unpack.")]
                                           [pair-val (car-vals cdr-vals) (loop (extend-env (car vars) car-vals env)
                                                                               (cdr vars)
                                                                               cdr-vals)]
                                           [else (eopl:error 'value-of "Expect a pair.")])))]
      [emptylist-exp () (emptylist-val)]
      [cond-exp (exps1 exps2) (let loop ([exps1 exps1]
                                         [exps2 exps2])
                                (if (null? exps1)
                                    (eopl:error 'value-of "All cond tests failed.")
                                    (let ([condition (value-of-bool-exp (car exps1) env)])
                                      (if (expval->bool condition)
                                          (value-of (car exps2) env)
                                          (loop (cdr exps1)
                                                (cdr exps2))))))]
      [unary-app-exp (rator exp1) (let ([val (value-of exp1 env)])
                                    (cond [(equal? rator "car") (cases expval val
                                                                  [pair-val (car cdr) car]
                                                                  [else (eopl:error 'value-of "Expect a pair.")])]
                                          [(equal? rator "cdr") (cases expval val
                                                                  [pair-val (car cdr) cdr]
                                                                  [else (eopl:error 'value-of "Expect a pair.")])]
                                          [(equal? rator "minus") (num-val (- (expval->num val)))]
                                          [(equal? rator "print")
                                           (let ([scheme-value (let loop ([val val])
                                                                 (cases expval val
                                                                   [num-val (value) value]
                                                                   [bool-val (boolean) boolean]
                                                                   [emptylist-val () '()]
                                                                   [pair-val (car cdr) (cons (loop car)
                                                                                             (loop cdr))]))])
                                             (display scheme-value)
                                             (newline)
                                             (num-val 1))]
                                          [else (eopl:error 'value-of-bool-exp "Unknown operator: ~s." rator)]))]
      [binary-app-exp (rator exp1 exp2) (let ([val1 (value-of exp1 env)]
                                              [val2 (value-of exp2 env)])
                                          (cond [(equal? rator "-") (num-val (- (expval->num val1) (expval->num val2)))]
                                                [(equal? rator "*") (num-val (* (expval->num val1) (expval->num val2)))]
                                                [(equal? rator "/") (num-val (quotient (expval->num val1)
                                                                                       (expval->num val2)))]
                                                [(equal? rator "+") (num-val (+ (expval->num val1) (expval->num val2)))]
                                                [(equal? rator "cons") (pair-val val1 val2)]
                                                [else (eopl:error 'value-of-bool-exp "Unknown operator: ~s." rator)]))]
      [n-ary-app-exp (rator exps) (let ([vals (map (lambda (e)
                                                     (value-of e env))
                                                   exps)])
                                    (cond [(equal? rator "list") (let loop ([vals vals])
                                                                   (if (null? vals)
                                                                       (emptylist-val)
                                                                       (pair-val (car vals) (loop (cdr vals)))))]
                                          [else (eopl:error 'value-of-bool-exp "Unknown operator: ~s." rator)]))]
      [a-bool-exp (exp1) (value-of-bool-exp exp1 env)])))

(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
      [bool-unary-app-exp (rator exp1) (let ([val (value-of exp1 env)])
                                         (cond [(equal? rator "null?") (bool-val (cases expval val
                                                                                   [emptylist-val () #t]
                                                                                   [else #f]))]
                                               [(equal? rator "zero?") (bool-val (zero? (expval->num val)))]
                                               [else (eopl:error 'value-of-bool-exp "Unknown operator: ~s." rator)]))]
      [bool-binary-app-exp (rator exp1 exp2)
                           (let ([val1 (value-of exp1 env)]
                                 [val2 (value-of exp2 env)])
                             (cond [(equal? rator "equal?") (bool-val (= (expval->num val1)
                                                                         (expval->num val2)))]
                                   [(equal? rator "greater?") (bool-val (> (expval->num val1)
                                                                           (expval->num val2)))]
                                   [(equal? rator "less?") (bool-val (< (expval->num val1)
                                                                        (expval->num val2)))]
                                   [else (eopl:error 'value-of-bool-exp "Unknown operator: ~s." rator)]))])))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (init-env))])))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(provide num-val bool-val emptylist-val pair-val run)
